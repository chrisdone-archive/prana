{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Import from GHC into Prana.

module Prana.Ghc
  ( fromGenStgTopBinding
  , runConvert
  , Convert
  , ConvertError(..)
  ) where

import qualified CoreSyn
import           Data.ByteString (ByteString)
import           Data.Int
import           Data.Maybe
import           Data.Validation
import qualified DataCon
import qualified FastString
import qualified Module
import qualified Name
import           Prana.Types
import qualified StgSyn
import qualified Unique
import qualified Var

--------------------------------------------------------------------------------
-- Convert monad

data ConvertError
  = UnexpectedPolymorphicCaseAlts
  | UnexpectedLambda
  | UnexpectedInternalName
  | UnknownVariable
  deriving (Show, Eq)

newtype Convert a =
  Convert
    { runConvert :: Validation [ConvertError] a
    }
  deriving (Functor, Applicative)

--------------------------------------------------------------------------------
-- Conversion functions

fromGenStgTopBinding :: StgSyn.GenStgTopBinding Var.Id Var.Id -> Convert GlobalBinding
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
    StgSyn.StgTopStringLit bindr byteString ->
      GlobalStringLit <$> lookupGlobalVarId bindr <*> pure byteString

fromGenStgBinding :: StgSyn.GenStgBinding Var.Id Var.Id -> Convert LocalBinding
fromGenStgBinding =
  \case
    StgSyn.StgNonRec bindr rhs ->
      LocalNonRec <$> lookupLocalVarId bindr <*> fromGenStgRhs rhs
    StgSyn.StgRec pairs ->
      LocalRec <$>
      traverse
        (\(bindr, rhs) -> (,) <$> lookupLocalVarId bindr <*> fromGenStgRhs rhs)
        pairs

fromGenStgRhs :: StgSyn.GenStgRhs Var.Id Var.Id -> Convert Rhs
fromGenStgRhs =
  \case
    StgSyn.StgRhsClosure _costCentreStack _binderInfo freeVariables updateFlag parameters expr ->
      RhsClosure <$> traverse lookupLocalVarId freeVariables <*>
      pure
        (case updateFlag of
           StgSyn.ReEntrant -> ReEntrant
           StgSyn.Updatable -> Updatable
           StgSyn.SingleEntry -> SingleEntry) <*>
      traverse lookupLocalVarId parameters <*>
      fromStgGenExpr expr
    StgSyn.StgRhsCon _costCentreStack dataCon arguments ->
      RhsCon <$> lookupDataConId dataCon <*> traverse fromStgGenArg arguments

fromStgGenArg :: StgSyn.GenStgArg Var.Id -> Convert Arg
fromStgGenArg =
  \case
    StgSyn.StgVarArg occ -> VarArg <$> lookupSomeVarId occ
    StgSyn.StgLitArg _literal -> pure (LitArg Lit)

fromStgGenExpr :: StgSyn.GenStgExpr Var.Id Var.Id -> Convert Expr
fromStgGenExpr =
  \case
    StgSyn.StgApp occ arguments ->
      AppExpr <$> lookupSomeVarId occ <*> traverse fromStgGenArg arguments
    StgSyn.StgLit literal -> LitExpr <$> pure (const Lit literal)
    StgSyn.StgConApp dataCon arguments types ->
      ConAppExpr <$> lookupDataConId dataCon <*> traverse fromStgGenArg arguments <*>
      pure (map (const Type) types)
    StgSyn.StgOpApp stgOp arguments typ ->
      OpAppExpr <$> pure (const Op stgOp) <*> traverse fromStgGenArg arguments <*>
      pure (const Type typ)
    StgSyn.StgCase expr bndr altType alts ->
      CaseExpr <$> fromStgGenExpr expr <*> lookupLocalVarId bndr <*>
      case altType of
        StgSyn.PolyAlt
          | [(CoreSyn.DEFAULT, [], rhs)] <- alts ->
            PolymorphicAlt <$> fromStgGenExpr rhs
          | otherwise -> Convert (Failure [UnexpectedPolymorphicCaseAlts])
        StgSyn.MultiValAlt count ->
          (\(mdef, dataAlts) -> MultiValAlts count dataAlts mdef) <$>
          fromAltTriples alts
        StgSyn.AlgAlt tyCon -> do
          (\(mdef, dataAlts) -> DataAlts (const TyCon tyCon) dataAlts mdef) <$>
            fromAltTriples alts
        StgSyn.PrimAlt primRep -> do
          (\(mdef, primAlts) -> PrimAlts (const PrimRep primRep) primAlts mdef) <$>
            fromPrimAltTriples alts
    StgSyn.StgLet binding expr ->
      LetExpr <$> fromGenStgBinding binding <*> fromStgGenExpr expr
    StgSyn.StgLetNoEscape binding expr ->
      LetExpr <$> fromGenStgBinding binding <*> fromStgGenExpr expr
    StgSyn.StgTick _tickish expr -> fromStgGenExpr expr
    StgSyn.StgLam {} -> Convert (Failure [UnexpectedLambda])

fromAltTriples :: [StgSyn.GenStgAlt Var.Id Var.Id] -> Convert (Maybe Expr, [DataAlt])
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

fromPrimAltTriples :: [StgSyn.GenStgAlt Var.Id Var.Id] -> Convert (Maybe Expr, [LitAlt])
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
         LitAlt <$> pure (const Lit dc) <*> traverse lookupLocalVarId bs <*>
         fromStgGenExpr e)
      adtAlts

--------------------------------------------------------------------------------
-- Bindings and names

lookupDataConId :: DataCon.DataCon -> Convert DataConId
lookupDataConId = error "lookupDataConId"

lookupSomeVarId :: Var.Id -> Convert SomeVarId
lookupSomeVarId varId =
  if Var.isExportedId varId
    then fmap SomeGlobalVarId (lookupGlobalVarId varId)
    else fmap SomeLocalVarId (lookupLocalVarId varId)

-- TODO: use toGlobalNormalized
lookupGlobalVarId :: Var.Id -> Convert GlobalVarId
lookupGlobalVarId = error "lookupGlobalVarId"

-- TODO: use toLocalNormalized
lookupLocalVarId :: Var.Id -> Convert LocalVarId
lookupLocalVarId = error "lookupLocalVarId"

--------------------------------------------------------------------------------
-- Names database

data GlobalNormalized =
  GlobalNormalized
    { globalNormalizedPackage :: {-# UNPACK #-}!ByteString
    , globalNormalizedModule :: {-# UNPACK #-}!ByteString
    , globalNormalizedName :: {-# UNPACK #-}!ByteString
    }
  deriving (Show, Ord, Eq)

data LocalNormalized =
  LocalNormalized
    { localNormalizedPackage :: {-# UNPACK #-}!ByteString
    , localNormalizedModule :: {-# UNPACK #-}!ByteString
    , localNormalizedName :: {-# UNPACK #-}!ByteString
    , localNormalizedUnique :: {-# UNPACK #-}!Unique
    }
  deriving (Show, Ord, Eq)

newtype ConstrNormalized =
  ConstrNormalized
    { unConstrNormalized :: GlobalNormalized
    }
  deriving (Show, Ord, Eq)

newtype Unique =
  Unique
    { unUnique :: Int64
    }
  deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------
-- Convert GHC ids to exported or local IDS

toConstrNormalized :: Module.Module -> DataCon.DataCon -> Convert ConstrNormalized
toConstrNormalized m =
  fmap ConstrNormalized . toGlobalNormalized m . DataCon.dataConWorkId

toGlobalNormalized :: Module.Module -> Var.Id -> Convert GlobalNormalized
toGlobalNormalized m thing =
  if Name.isInternalName name
    then Convert (Failure [UnexpectedInternalName])
    else pure (GlobalNormalized package module' name')
  where
    package =
      FastString.fs_bs
        (Module.unitIdFS (Module.moduleUnitId (Name.nameModule name)))
    module' =
      FastString.fs_bs
        (Module.moduleNameFS (Module.moduleName (Name.nameModule name)))
    name' = FastString.fs_bs (Name.getOccFS name)
    name =
      case Name.nameModule_maybe n of
        Nothing -> qualifyName m n
        Just {} -> n
      where
        n = Name.getName thing

toLocalNormalized :: Module.Module -> Var.Id -> Convert LocalNormalized
toLocalNormalized m thing =
  if Name.isInternalName name
    then Convert (Failure [UnexpectedInternalName])
    else pure
           (LocalNormalized
              package
              module'
              name'
              (Unique (fromIntegral (Unique.getKey (Unique.getUnique name)))))
  where
    package =
      FastString.fs_bs
        (Module.unitIdFS (Module.moduleUnitId (Name.nameModule name)))
    module' =
      FastString.fs_bs
        (Module.moduleNameFS (Module.moduleName (Name.nameModule name)))
    name' = FastString.fs_bs (Name.getOccFS name)
    name =
      case Name.nameModule_maybe n of
        Nothing -> qualifyName m n
        Just {} -> n
      where
        n = Name.getName thing

qualifyName :: Module.Module -> Name.Name -> Name.Name
qualifyName m name =
  Name.mkExternalName
    (Unique.getUnique name)
    m
    (Name.nameOccName name)
    (Name.nameSrcSpan name)
