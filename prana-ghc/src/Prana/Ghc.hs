{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Import from GHC into Prana.

module Prana.Ghc
  ( fromGenStgTopBinding
  ) where

import           Data.Functor.Identity
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

fromStgGenExpr :: StgSyn.GenStgExpr bndr occ -> Convert Expr
fromStgGenExpr = undefined

--------------------------------------------------------------------------------
-- Bindings and names

getSomeVarId :: Var.Id -> Convert SomeVarId
getSomeVarId = error "getSomeVarId"

getGlobalVarId :: Var.Id -> Convert GlobalVarId
getGlobalVarId = error "getGlobalVarId"

getLocalVarId :: Var.Id -> Convert LocalVarId
getLocalVarId = error "getLocalVarId"
