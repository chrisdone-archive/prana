{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Import from GHC into Prana.

module Prana.Ghc
  ( fromGenStgTopBinding
  ) where

import qualified CoreSyn
import           Data.Functor.Identity
import           Data.Maybe
import qualified DataCon
import           Prana.Types
import qualified StgSyn
import qualified Var

--------------------------------------------------------------------------------
-- Convert monad

newtype Convert a = Convert (Identity a)
  deriving (Functor, Monad, Applicative)

--------------------------------------------------------------------------------
-- Conversion functions

fromGenStgTopBinding :: StgSyn.GenStgTopBinding Var.Id Var.Id -> Convert GlobalBinding
fromGenStgTopBinding =
  \case
    StgSyn.StgTopLifted genStdBinding ->
      case genStdBinding of
        StgSyn.StgNonRec bindr rhs ->
          GlobalNonRec <$> getGlobalVarId bindr <*> fromGenStgRhs rhs
        StgSyn.StgRec pairs ->
          GlobalRec <$>
          mapM
            (\(bindr, rhs) -> (,) <$> getGlobalVarId bindr <*> fromGenStgRhs rhs)
            pairs
    StgSyn.StgTopStringLit bindr byteString ->
      GlobalStringLit <$> getGlobalVarId bindr <*> pure byteString

fromGenStgBinding :: StgSyn.GenStgBinding Var.Id Var.Id -> Convert LocalBinding
fromGenStgBinding =
  \case
    StgSyn.StgNonRec bindr rhs ->
      LocalNonRec <$> getLocalVarId bindr <*> fromGenStgRhs rhs
    StgSyn.StgRec pairs ->
      LocalRec <$>
      mapM
        (\(bindr, rhs) -> (,) <$> getLocalVarId bindr <*> fromGenStgRhs rhs)
        pairs

fromGenStgRhs :: StgSyn.GenStgRhs Var.Id Var.Id -> Convert Rhs
fromGenStgRhs =
  \case
    StgSyn.StgRhsClosure _costCentreStack _binderInfo freeVariables updateFlag parameters expr ->
      RhsClosure <$> mapM getLocalVarId freeVariables <*>
      pure
        (case updateFlag of
           StgSyn.ReEntrant -> ReEntrant
           StgSyn.Updatable -> Updatable
           StgSyn.SingleEntry -> SingleEntry) <*>
      mapM getLocalVarId parameters <*>
      fromStgGenExpr expr
    StgSyn.StgRhsCon _costCentreStack _dataCon arguments ->
      RhsCon <$> pure DataCon <*> mapM fromStgGenArg arguments

fromStgGenArg :: StgSyn.GenStgArg Var.Id -> Convert Arg
fromStgGenArg =
  \case
    StgSyn.StgVarArg occ -> VarArg <$> getSomeVarId occ
    StgSyn.StgLitArg _literal -> pure (LitArg Lit)

fromStgGenExpr :: StgSyn.GenStgExpr Var.Id Var.Id -> Convert Expr
fromStgGenExpr =
  \case
    StgSyn.StgApp occ arguments ->
      AppExpr <$> fromId occ <*> mapM fromStgGenArg arguments
    StgSyn.StgLit literal -> LitExpr <$> pure (const Lit literal)
    StgSyn.StgConApp dataCon arguments types ->
      ConAppExpr <$> fromDataCon dataCon <*> mapM fromStgGenArg arguments <*>
      pure (map (const Type) types)
    StgSyn.StgOpApp stgOp arguments typ ->
      OpAppExpr <$> pure (const Op stgOp) <*> mapM fromStgGenArg arguments <*>
      pure (const Type typ)
    StgSyn.StgCase expr bndr altType alts ->
      CaseExpr <$> fromStgGenExpr expr <*> getLocalVarId bndr <*>
      case altType of
        StgSyn.PolyAlt
          | [(CoreSyn.DEFAULT, [], rhs)] <- alts ->
            PolymorphicAlt <$> fromStgGenExpr rhs
          | otherwise -> error "Unexpected polymorphic case alts."
        StgSyn.MultiValAlt count -> do
          (mdef, dataAlts) <- fromAltTriples alts
          pure (MultiValAlts count dataAlts mdef)
        StgSyn.AlgAlt tyCon -> do
          (mdef, dataAlts) <- fromAltTriples alts
          pure (DataAlts (const TyCon tyCon) dataAlts mdef)
        StgSyn.PrimAlt primRep -> do
          (mdef, primAlts) <- fromPrimAltTriples alts
          pure (PrimAlts (const PrimRep primRep) primAlts mdef)
    StgSyn.StgLet binding expr ->
      LetExpr <$> fromGenStgBinding binding <*> fromStgGenExpr expr
    StgSyn.StgLetNoEscape binding expr ->
      LetExpr <$> fromGenStgBinding binding <*> fromStgGenExpr expr
    StgSyn.StgTick _tickish expr -> fromStgGenExpr expr
    StgSyn.StgLam {} -> error "Lambda should not exist at this point."

fromAltTriples :: [StgSyn.GenStgAlt Var.Id Var.Id] -> Convert (Maybe Expr, [DataAlt])
fromAltTriples alts = do
  let mdef =
        listToMaybe
          (mapMaybe
             (\case
                (CoreSyn.DEFAULT, [], e) -> pure e
                _ -> Nothing)
             alts)
  undefined

fromPrimAltTriples :: [StgSyn.GenStgAlt Var.Id Var.Id] -> Convert (Maybe Expr, [LitAlt])
fromPrimAltTriples = undefined

fromId :: Var.Id -> Convert SomeVarId
fromId = undefined

fromDataCon :: DataCon.DataCon -> Convert DataCon
fromDataCon = undefined

--------------------------------------------------------------------------------
-- Bindings and names

getSomeVarId :: Var.Id -> Convert SomeVarId
getSomeVarId = error "getSomeVarId"

getGlobalVarId :: Var.Id -> Convert GlobalVarId
getGlobalVarId = error "getGlobalVarId"

getLocalVarId :: Var.Id -> Convert LocalVarId
getLocalVarId = error "getLocalVarId"
