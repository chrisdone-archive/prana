{-# LANGUAGE LambdaCase #-}

-- |

module Prana.Interpret where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Int
import           Data.List
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Prana.Types

type LocalEnv = HashMap Int64 Exp

type GlobalEnv = Vector Exp

data WHNF
  = LamW !LocalEnv !Int64 !Exp
  | LitW !Lit
  | ConW !(Vector Thunk)
  deriving (Show, Eq)

data Thunk =
  Thunk !LocalEnv !Exp
  deriving (Show, Eq)

eval :: GlobalEnv -> LocalEnv -> Exp -> IO WHNF
eval global local =
  \case
    LamE (LocalIndex param) body -> pure (LamW local param body)
    LetE binds body ->
      eval
        global
        (foldl'
           (\localEnv bind ->
              case bindVar bind of
                LocalIndex i -> HM.insert i (bindExp bind) localEnv
                ExportedIndex {} -> error "eval.LetE.ExportedIndex: make unrepresentable.")
           local
           binds)
        body
    VarE var ->
      case var of
        ExportedIndex i ->
          case global V.!? (fromIntegral i) of
            Nothing -> error "eval.VarE.ExportedIndex = Nothing"
            Just e -> eval global local e
        LocalIndex i ->
          case HM.lookup i local of
            Nothing -> error "eval.VarE.LocalIndex = Nothing"
            Just e -> eval global local e
    AppE func arg -> do
      result <- eval global local func
      case result of
        LamW env param body -> eval global (HM.insert param arg env) body
        LitW lit -> error ("eval.AppE.LitW: expected function: " ++ show lit)
        ConW args -> pure (ConW (args <> V.singleton (Thunk local arg)))
    LitE lit -> pure (LitW lit)
    LamE ExportedIndex {} _ ->
      error "eval.LamE.ExportedIndex: make unrepresentable."
    CaseE {} -> error "eval.CaseE: undefined"
    ConE {} -> pure (ConW mempty)
    FFIE {} -> error "eval.FFIE: undefined"
    WiredInE {} -> error "eval.WiredInE: undefined"
    PrimOpE {} -> error "eval.PrimOpE: undefined"
    MethodE {} -> error "eval.MethodE: undefined"
    DictE {} -> error "eval.DictE: undefined"
