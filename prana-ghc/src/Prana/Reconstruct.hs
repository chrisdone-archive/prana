{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

-- | Reconstruct the AST by taking the GHC STG AST and producing
-- Prana's own ASt.

module Prana.Reconstruct
  ( fromGenStgTopBinding
  , runConvert
  , Scope(..)
  ) where

import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import qualified CoreSyn
import           Data.Map.Strict (Map)
import           Data.Maybe
import qualified DataCon
import           Prana.Rename
import           Prana.Types
import qualified StgSyn

data Scope =
  Scope
    { scopeGlobals :: Map Name GlobalVarId
    , scopeLocals :: Map Name LocalVarId
    , scopeDataCons :: Map Name DataConId
    }

-- | A conversion monad.
newtype Convert a =
  Convert
    { runConvert :: ReaderT Scope (Either ConvertError) a
    }
  deriving (Functor, Applicative, Monad)

-- | An error while converting the AST.
data ConvertError
  = UnexpectedPolymorphicCaseAlts
  | UnexpectedLambda
  deriving (Show, Eq)

-- | Produce a failure.
failure :: ConvertError -> Convert a
failure = Convert . lift . Left

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
    StgSyn.StgTopStringLit bindr byteString ->
      GlobalStringLit <$> lookupGlobalVarId bindr <*> pure byteString

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

fromStgGenArg :: StgSyn.GenStgArg Name -> Convert Arg
fromStgGenArg =
  \case
    StgSyn.StgVarArg occ -> VarArg <$> lookupSomeVarId occ
    StgSyn.StgLitArg _literal -> pure (LitArg Lit)

fromStgGenExpr :: StgSyn.GenStgExpr Name Name -> Convert Expr
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
          | otherwise -> failure UnexpectedPolymorphicCaseAlts
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
    StgSyn.StgLam {} -> failure UnexpectedLambda

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
         LitAlt <$> pure (const Lit dc) <*> traverse lookupLocalVarId bs <*>
         fromStgGenExpr e)
      adtAlts

--------------------------------------------------------------------------------
-- Lookup functions

lookupSomeVarId :: Name -> Convert SomeVarId
lookupSomeVarId = undefined

lookupGlobalVarId :: Name -> Convert GlobalVarId
lookupGlobalVarId = undefined

lookupLocalVarId :: Name -> Convert LocalVarId
lookupLocalVarId = undefined

lookupDataConId :: DataCon.DataCon -> Convert DataConId
lookupDataConId = undefined
