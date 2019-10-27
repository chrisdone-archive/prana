{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Reconstruct the AST by taking the GHC STG AST and producing
-- Prana's own ASt.

module Prana.Reconstruct
  ( fromGenStgTopBinding
  , runConvert
  , Scope(..)
  , ConvertError (..)
  , failure
  ) where

import qualified BasicTypes
import           Control.Exception
import           Control.Monad.Trans.Reader
import qualified CoreSyn
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as S8
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Typeable
import           Data.Validation
import qualified DataCon
import           Debug.Trace
import qualified FastString
import           ForeignCall
import qualified Literal
import qualified Module
import qualified Name
import qualified Outputable (ppr, showSDocUnsafe)
import           Prana.FFI
import           Prana.Index
import           Prana.Rename
import           Prana.Types as Prana
import qualified PrimOp
import qualified StgSyn
import           Text.Read
import qualified TyCon
import qualified Type
import qualified Unique

-- | A conversion monad.
newtype Convert a =
  Convert
    { runConvert :: ReaderT Scope (Validation (NonEmpty ConvertError)) a
    }
  deriving (Functor, Applicative)

-- | An error while converting the AST.
data ConvertError
  = UnexpectedPolymorphicCaseAlts
  | UnexpectedLambda
  | ConNameNotFound !Name
  | GlobalNameNotFound !Name
  | LocalNameNotFound !Name
  | SomeNameNotFound !Name
  | RenameDataConError !DataCon.DataCon !RenameFailure
  | RenameFailure !RenameFailure
  | BadOpConversion !PrimOp.PrimOp
  | TypeNameNotFound !Name
  | CouldntGetTyConForPrimOp !String
  | UnsupportedCallingConvention !ForeignCall.CCallConv
  | FFITypeError !FFIError
  deriving (Eq, Typeable)

instance Exception ConvertError where
  displayException =
    \case
      SomeNameNotFound name -> "Variable name not found: " <> displayName name
      LocalNameNotFound name -> "Local variable name not found: " <> displayName name
      GlobalNameNotFound name -> "Global variable name not found: " <> displayName name
      ConNameNotFound name -> "Constructor name not found: " <> displayName name
      orelse -> show orelse

instance Show ConvertError where
  show (FFITypeError err) = "FFITypeError " ++ show err
  show UnexpectedPolymorphicCaseAlts {} = "UnexpectedPolymorphicCaseAlts"
  show UnexpectedLambda {} = "UnexpectedLambda"
  show (ConNameNotFound name) = "ConNameNotFound " ++ show name
  show (LocalNameNotFound name) = "LocalNameNotFound " ++ show name
  show (GlobalNameNotFound name) = "GlobalNameNotFound " ++ show name
  show (SomeNameNotFound name) = "SomeNameNotFound " ++ show name
  show RenameDataConError {} = "RenameDataConError"
  show RenameFailure {} = "RenameFailure"
  show (CouldntGetTyConForPrimOp s) = "Couldn'tGetTyConForPrimOp " ++ show s
  show (BadOpConversion primop) = "BadOpConversion " ++ show primop
  show (TypeNameNotFound primop) = "TypeNameNotFound " ++ show primop
  show (UnsupportedCallingConvention callconv) =
    "UnsupportedCallingConvention " ++ show callconv

data Scope =
  Scope
    { scopeIndex :: !Index
    , scopeModule :: !Module.Module
    }

-- | Produce a failure.
failure :: ConvertError -> Convert a
failure e = Convert (ReaderT (\_ -> Failure (pure e)))

-- | Embed a validation within a Convert.
embedValidation :: Validation (NonEmpty ConvertError) a -> Convert a
embedValidation m = Convert (ReaderT (const m))

--------------------------------------------------------------------------------
-- Conversion functions

fromGenStgTopBinding :: StgSyn.GenStgTopBinding Name Name -> Convert GlobalBinding
fromGenStgTopBinding =
  \case
    StgSyn.StgTopLifted genStdBinding ->
      case genStdBinding of
        StgSyn.StgNonRec bindr rhs ->
          GlobalNonRec <$> lookupGlobalVarId bindr <*> fromGenStgRhs rhs
        StgSyn.StgRec pairs ->
          GlobalRec <$>
          traverse
            (\(bindr, rhs) -> (,) <$> lookupGlobalVarId bindr <*> fromGenStgRhs rhs)
            pairs
    StgSyn.StgTopStringLit bindr byteString' ->
      GlobalStringLit <$> lookupGlobalVarId bindr <*> pure byteString'

fromGenStgBinding :: StgSyn.GenStgBinding Name Name -> Convert LocalBinding
fromGenStgBinding =
  \case
    StgSyn.StgNonRec bindr rhs ->
      LocalNonRec <$> lookupLocalVarId bindr <*> fromGenStgRhs rhs
    StgSyn.StgRec pairs ->
      LocalRec <$>
      traverse
        (\(bindr, rhs) -> (,) <$> lookupLocalVarId bindr <*> fromGenStgRhs rhs)
        pairs

fromGenStgRhs :: StgSyn.GenStgRhs Name Name -> Convert Rhs
fromGenStgRhs =
  \case
    StgSyn.StgRhsClosure _costCentreStack _binderInfo freeVariables updateFlag parameters expr ->
      RhsClosure <$>
      (Closure <$> traverse lookupLocalVarId freeVariables <*>
       pure
         (case updateFlag of
            StgSyn.ReEntrant -> ReEntrant
            StgSyn.Updatable -> Updatable
            StgSyn.SingleEntry -> SingleEntry) <*>
       traverse lookupLocalVarId parameters <*>
       fromStgGenExpr expr)
    StgSyn.StgRhsCon _costCentreStack dataCon arguments ->
      RhsCon <$> (Con <$> lookupDataConId dataCon <*> traverse fromStgGenArg arguments)

fromStgGenArg :: StgSyn.GenStgArg Name -> Convert Arg
fromStgGenArg =
  \case
    StgSyn.StgVarArg occ -> VarArg <$> lookupSomeVarId occ
    StgSyn.StgLitArg literal ->
      pure
        (LitArg
           (fromLiteral literal))

fromLiteral :: Literal.Literal -> Lit
fromLiteral =
  \case
    Literal.MachInt integer -> IntLit (fromIntegral integer)
    Literal.MachChar chareger -> CharLit (chareger)
    Literal.MachStr x -> StringLit x
    Literal.MachNullAddr -> NullAddrLit
    Literal.MachInt64 x -> Int64Lit (fromIntegral x)
    Literal.MachWord x -> WordLit (fromIntegral x)
    Literal.MachWord64 x -> Word64Lit (fromIntegral x)
    Literal.MachFloat x -> FloatLit (realToFrac x)
    Literal.MachDouble x -> DoubleLit (realToFrac x)
    Literal.MachLabel _ _ _ -> LabelLit
    Literal.LitInteger i _ -> IntegerLit i

fromStgGenExpr :: StgSyn.GenStgExpr Name Name -> Convert Expr
fromStgGenExpr =
  \case
    StgSyn.StgApp occ arguments ->
      AppExpr <$> lookupSomeVarId occ <*> traverse fromStgGenArg arguments
    StgSyn.StgLit literal -> LitExpr <$> pure (fromLiteral literal)
    StgSyn.StgConApp dataCon arguments types ->
      ConAppExpr <$> lookupDataConId dataCon <*>
      traverse fromStgGenArg arguments <*>
      pure (map (const Type) types)
    StgSyn.StgOpApp stgOp arguments typ ->
      OpAppExpr <$> fromStgOp typ stgOp <*> traverse fromStgGenArg arguments
    StgSyn.StgCase expr bndr altType alts ->
      CaseExpr <$> fromStgGenExpr expr <*> lookupLocalVarId bndr <*>
      case altType of
        StgSyn.PolyAlt
          | [(CoreSyn.DEFAULT, [], rhs)] <- alts ->
            PolymorphicAlt <$> fromStgGenExpr rhs
          | otherwise -> failure UnexpectedPolymorphicCaseAlts
        StgSyn.MultiValAlt count ->
          (\(mdef, dataAlts) -> MultiValAlts count dataAlts mdef) <$>
          fromAltTriples alts
        StgSyn.AlgAlt tyCon -> do
          (\(mdef, dataAlts) -> DataAlts (const TyCon tyCon) dataAlts mdef) <$>
            fromAltTriples alts
        StgSyn.PrimAlt primRep -> do
          (\(mdef, primAlts) -> PrimAlts (fromPrimRep primRep) primAlts mdef) <$>
            fromPrimAltTriples alts
    StgSyn.StgLet binding expr ->
      LetExpr <$> fromGenStgBinding binding <*> fromStgGenExpr expr
    StgSyn.StgLetNoEscape binding expr ->
      LetExpr <$> fromGenStgBinding binding <*> fromStgGenExpr expr
    StgSyn.StgTick _tickish expr -> fromStgGenExpr expr
    StgSyn.StgLam {} -> failure UnexpectedLambda

fromStgOp :: Type.Type -> StgSyn.StgOp -> Convert Op
fromStgOp typ =
  \case
    StgSyn.StgPrimOp op ->
      PrimOp <$> fromPrimOp op <*>
      case Type.tyConAppTyConPicky_maybe typ of
        Just ty -> fmap Just (lookupTypeId (Name.getName ty))
        Nothing -> pure Nothing
    StgSyn.StgFCallOp (foreignCall) unique' ->
      Prana.ForeignOp <$>
      fromForeignCall
        foreignCall
        (Unexported (fromIntegral (Unique.getKey unique'))) <*>
      embedValidation (first (fmap FFITypeError) (parseAcceptableFFIReturnType typ))
    StgSyn.StgPrimCallOp primCall -> trace (show primCall) (pure OtherOp)

fromForeignCall :: ForeignCall.ForeignCall -> Unique -> Convert Prana.CCallSpec
fromForeignCall (ForeignCall.CCall (ForeignCall.CCallSpec target conv safety')) unique' =
  Prana.CCallSpec <$>
  (pure
     (case target of
        ForeignCall.DynamicTarget -> Prana.DynamicTarget
        ForeignCall.StaticTarget str bs _ bool ->
          Prana.StaticTarget
            (StaticCallTarget
               { sourceText =
                   case str of
                     BasicTypes.SourceText s -> Prana.SourceText s
                     BasicTypes.NoSourceText -> Prana.NoSourceText
               , byteString = FastString.fs_bs bs
               , functionOrValue =
                   if bool
                     then IsFunction
                     else IsValue
               }))) <*>
  (case conv of
     ForeignCall.CCallConv -> pure Prana.CCallConv
     ForeignCall.CApiConv -> pure Prana.CApiConv
     ForeignCall.StdCallConv -> pure Prana.StdCallConv
     _ -> failure (UnsupportedCallingConvention conv)) <*>
  pure
    (case safety' of
       ForeignCall.PlaySafe -> Prana.PlaySafe
       ForeignCall.PlayInterruptible -> Prana.PlayInterruptible
       ForeignCall.PlayRisky -> Prana.PlayRisky) <*>
  pure unique'

instance Show Type.Type where
  show n = Outputable.showSDocUnsafe (Outputable.ppr n)

deriving instance Show PrimOp.PrimCall
deriving instance Show ForeignCall.ForeignCall
deriving instance Show ForeignCall.CCallSpec
deriving instance Show ForeignCall.CCallTarget
deriving instance Show ForeignCall.CCallConv
deriving instance Show PrimOp.PrimOp
deriving instance Show PrimOp.PrimOpVecCat

-- | The constructor names are the same.
fromPrimOp :: PrimOp.PrimOp -> Convert PrimOp
fromPrimOp op =
  case readMaybe (show op) of
    Nothing -> failure (BadOpConversion op)
    Just pr -> pure pr

fromPrimRep :: TyCon.PrimRep -> PrimRep
fromPrimRep =
  \case
    TyCon.VoidRep -> VoidRep
    TyCon.LiftedRep -> LiftedRep
    TyCon.UnliftedRep -> UnliftedRep
    TyCon.IntRep -> IntRep
    TyCon.WordRep -> WordRep
    TyCon.Int64Rep -> Int64Rep
    TyCon.Word64Rep -> Word64Rep
    TyCon.AddrRep -> AddrRep
    TyCon.FloatRep -> FloatRep
    TyCon.DoubleRep -> DoubleRep
    TyCon.VecRep size primElemRep -> VecRep size (fromPrimElemRep primElemRep)

fromPrimElemRep :: TyCon.PrimElemRep -> PrimElemRep
fromPrimElemRep =
  \case
    TyCon.Int8ElemRep -> Int8ElemRep
    TyCon.Int16ElemRep -> Int16ElemRep
    TyCon.Int32ElemRep -> Int32ElemRep
    TyCon.Int64ElemRep -> Int64ElemRep
    TyCon.Word8ElemRep -> Word8ElemRep
    TyCon.Word16ElemRep -> Word16ElemRep
    TyCon.Word32ElemRep -> Word32ElemRep
    TyCon.Word64ElemRep -> Word64ElemRep
    TyCon.FloatElemRep -> FloatElemRep
    TyCon.DoubleElemRep -> DoubleElemRep

fromAltTriples :: [StgSyn.GenStgAlt Name Name] -> Convert (Maybe Expr, [DataAlt])
fromAltTriples alts = do
  let mdef =
        listToMaybe
          (mapMaybe
             (\case
                (CoreSyn.DEFAULT, [], e) -> Just e
                _ -> Nothing)
             alts)
      adtAlts =
        mapMaybe
          (\case
             (CoreSyn.DataAlt dc, bs, e) -> pure (dc, bs, e)
             _ -> Nothing)
          alts
  (,) <$> maybe (pure Nothing) (fmap Just . fromStgGenExpr) mdef <*>
    traverse
      (\(dc, bs, e) ->
         DataAlt <$> lookupDataConId dc <*> traverse lookupLocalVarId bs <*>
         fromStgGenExpr e)
      adtAlts

fromPrimAltTriples :: [StgSyn.GenStgAlt Name Name] -> Convert (Maybe Expr, [LitAlt])
fromPrimAltTriples alts = do
  let mdef =
        listToMaybe
          (mapMaybe
             (\case
                (CoreSyn.DEFAULT, [], e) -> Just e
                _ -> Nothing)
             alts)
      adtAlts =
        mapMaybe
          (\case
             (CoreSyn.LitAlt dc, bs, e) -> pure (dc, bs, e)
             _ -> Nothing)
          alts
  (,) <$> maybe (pure Nothing) (fmap Just . fromStgGenExpr) mdef <*>
    traverse
      (\(dc, bs, e) ->
         LitAlt <$> pure (fromLiteral dc) <*> traverse lookupLocalVarId bs <*>
         fromStgGenExpr e)
      adtAlts

--------------------------------------------------------------------------------
-- Lookup functions

lookupSomeVarId :: Name -> Convert SomeVarId
lookupSomeVarId name =
  asking
    (\scope ->
       case M.lookup name wiredInVals of
         Just wiredIn -> pure (WiredInVal wiredIn)
         Nothing ->
           case M.lookup name (indexGlobals (scopeIndex scope)) of
             Nothing ->
               case M.lookup name (indexLocals (scopeIndex scope)) of
                 Nothing -> Failure (pure (SomeNameNotFound name))
                 Just g -> pure (SomeLocalVarId g)
             Just g -> pure (SomeGlobalVarId g))

lookupGlobalVarId :: Name -> Convert GlobalVarId
lookupGlobalVarId name =
  asking
    (\scope ->
       case M.lookup name (indexGlobals (scopeIndex scope)) of
         Nothing -> Failure (pure (GlobalNameNotFound name))
         Just g -> pure g)

lookupLocalVarId :: Name -> Convert LocalVarId
lookupLocalVarId name =
  asking
    (\scope ->
       case M.lookup name (indexLocals (scopeIndex scope)) of
         Nothing -> Failure (pure (LocalNameNotFound name))
         Just g -> pure g)

lookupDataConId :: DataCon.DataCon -> Convert DataConId
lookupDataConId dataCon =
  asking
    (\scope ->
       either
         (Failure . pure . RenameDataConError dataCon)
         (\name ->
            case M.lookup name unboxedTupleCons of
              Just wiredCon -> pure wiredCon
              Nothing ->
                case M.lookup name (indexDataCons (scopeIndex scope)) of
                  Nothing -> Failure (pure (ConNameNotFound name))
                  Just g -> pure g)
         (renameId (scopeModule scope) (DataCon.dataConWorkId dataCon)))

lookupTypeId :: Name.Name -> Convert TypeId
lookupTypeId name =
  asking
    (\scope ->
       either
         (Failure . pure . RenameFailure)
         (\ourName ->
            case M.lookup ourName wiredInTypes of
              Nothing ->
                case M.lookup ourName (indexTypes (scopeIndex scope)) of
                  Nothing -> Failure (pure (TypeNameNotFound ourName))
                  Just g -> pure g
              Just wiredInType -> pure (WiredInType wiredInType))
         (renameName (scopeModule scope) name))

-- | A way of injecting @ask@ into the Applicative.
asking :: (Scope -> Validation (NonEmpty ConvertError) a) -> Convert a
asking f = Convert (ReaderT f)

--------------------------------------------------------------------------------
-- Wired-in names

wiredInVals :: Map Name WiredInVal
wiredInVals =
  M.fromList
    [ ( Name
          { namePackage = "ghc-prim"
          , nameModule = "GHC.Prim"
          , nameName = "magicDict"
          , nameUnique = Exported
          }
      , WiredIn_magicDict),
      ( Name
          { namePackage = "ghc-prim"
          , nameModule = "GHC.Prim"
          , nameName = "void#"
          , nameUnique = Exported
          }
      , WiredIn_void#)
    , ( Name
          { namePackage = "ghc-prim"
          , nameModule = "GHC.Prim"
          , nameName = "coercionToken#"
          , nameUnique = Exported
          }
      , WiredIn_coercionToken#)
    , ( Name
          { namePackage = "ghc-prim"
          , nameModule = "GHC.Prim"
          , nameName = "realWorld#"
          , nameUnique = Exported
          }
      , WiredIn_realWorld#)
    ,  ( Name
           { namePackage = "ghc-prim"
           , nameModule = "GHC.Prim"
           , nameName = "nullAddr#"
           , nameUnique = Exported
           }
       , WiredIn_nullAddr#)
    , ( Name
          { namePackage = "ghc-prim"
          , nameModule = "GHC.Prim"
          , nameName = "seq"
          , nameUnique = Exported
          }
      , WiredIn_seq)
    , ( Name
          { namePackage = "base"
          , nameModule = "Control.Exception.Base"
          , nameName = "patError"
          , nameUnique = Exported
          }
      , WiredIn_patError)
    ,  ( Name
           { namePackage = "ghc-prim"
           , nameModule = "GHC.Prim"
           , nameName = "proxy#"
           , nameUnique = Exported
           }
       , WiredIn_proxy#)
    ]

-- Notes on tuples.
-- Copied from the GHC codebase:

-- However the /naming/ of the type/data constructors for one-tuples is a
-- bit odd:
--   3-tuples:  (,,)   (,,)#
--   2-tuples:  (,)    (,)#
--   1-tuples:  ??
--   0-tuples:  ()     ()#

-- Zero-tuples have used up the logical name. So we use 'Unit' and 'Unit#'
-- for one-tuples.  So in ghc-prim:GHC.Tuple we see the declarations:
--   data ()     = ()
--   data Unit a = Unit a
--   data (a,b)  = (a,b)

unboxedTupleCons :: Map Name DataConId
unboxedTupleCons =
  M.fromList
    (concat
       [ [ ( Name
               { namePackage = "ghc-prim"
               , nameModule = "GHC.Prim"
               , nameName = "(##)"
               , nameUnique = Exported
               }
           , UnboxedTupleConId 0)
         , ( Name
               { namePackage = "ghc-prim"
               , nameModule = "GHC.Prim"
               , nameName = "Unit#"
               , nameUnique = Exported
               }
           , UnboxedTupleConId 1)
         ]
       , [ ( Name
               { namePackage = "ghc-prim"
               , nameModule = "GHC.Prim"
               , nameName = "(#" <> S8.replicate (n - 1) ',' <> "#)"
               , nameUnique = Exported
               }
           , UnboxedTupleConId n)
         | n <- [2 .. 64]
         ]
       ])

--------------------------------------------------------------------------------
-- Wired in types

wiredInTypes :: Map Name WiredInType
wiredInTypes =
  M.fromList
    (concat
       [ [ ( Name
               { namePackage = "ghc-prim"
               , nameModule = "GHC.Prim"
               , nameName = "(##)"
               , nameUnique = Exported
               }
           , WiredIn_UnboxedTuple 0)
         , ( Name
               { namePackage = "ghc-prim"
               , nameModule = "GHC.Prim"
               , nameName = "Unit#"
               , nameUnique = Exported
               }
           , WiredIn_UnboxedTuple 1)
         ]
       , [ ( Name
               { namePackage = "ghc-prim"
               , nameModule = "GHC.Prim"
               , nameName = "(#" <> S8.replicate (n - 1) ',' <> "#)"
               , nameUnique = Exported
               }
           , WiredIn_UnboxedTuple n)
         | n <- [2 .. 64]
         ]
       ] ++
     map
       (first toName)
       [ ("Char#", WiredIn_CharPrimTyConName)
       , ("Int#", WiredIn_IntPrimTyConName)
       , ("Int32#", WiredIn_Int32PrimTyConName)
       , ("Int64#", WiredIn_Int64PrimTyConName)
       , ("Word#", WiredIn_WordPrimTyConName)
       , ("Word32#", WiredIn_Word32PrimTyConName)
       , ("Word64#", WiredIn_Word64PrimTyConName)
       , ("Addr#", WiredIn_AddrPrimTyConName)
       , ("Float#", WiredIn_FloatPrimTyConName)
       , ("Double#", WiredIn_DoublePrimTyConName)
       , ("State#", WiredIn_StatePrimTyConName)
       , ("Void#", WiredIn_VoidPrimTyConName)
       , ("Proxy#", WiredIn_ProxyPrimTyConName)
       , ("~#", WiredIn_EqPrimTyConName)
       , ("~R#", WiredIn_EqReprPrimTyConName)
       , ("~P#", WiredIn_EqPhantPrimTyConName)
       , ("RealWorld", WiredIn_RealWorldTyConName)
       , ("Array#", WiredIn_ArrayPrimTyConName)
       , ("ByteArray#", WiredIn_ByteArrayPrimTyConName)
       , ("ArrayArray#", WiredIn_ArrayArrayPrimTyConName)
       , ("SmallArray#", WiredIn_SmallArrayPrimTyConName)
       , ("MutableArray#", WiredIn_MutableArrayPrimTyConName)
       , ("MutableByteArray#", WiredIn_MutableByteArrayPrimTyConName)
       , ("MutableArrayArray#", WiredIn_MutableArrayArrayPrimTyConName)
       , ("SmallMutableArray#", WiredIn_SmallMutableArrayPrimTyConName)
       , ("MutVar#", WiredIn_MutVarPrimTyConName)
       , ("MVar#", WiredIn_MVarPrimTyConName)
       , ("TVar#", WiredIn_TVarPrimTyConName)
       , ("StablePtr#", WiredIn_StablePtrPrimTyConName)
       , ("StableName#", WiredIn_StableNamePrimTyConName)
       , ("Compact#", WiredIn_CompactPrimTyConName)
       , ("BCO#", WiredIn_BcoPrimTyConName)
       , ("Weak#", WiredIn_WeakPrimTyConName)
       , ("ThreadId#", WiredIn_ThreadIdPrimTyConName)
       ])
  where
    toName str =
      Name
        { namePackage = "ghc-prim"
        , nameModule = "GHC.Prim"
        , nameName = str
        , nameUnique = Exported
        }
