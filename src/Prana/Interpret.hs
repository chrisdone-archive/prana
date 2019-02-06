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

type GlobalEnv = HashMap Int64 Exp

type MethodEnv = HashMap Int64 Int64

data WHNF
  = LamW !LocalEnv !Int64 !Exp
  | LitW !Lit
  | ConW !ConId !(Vector Thunk)
  deriving (Show, Eq)

data Thunk =
  Thunk !LocalEnv !Exp
  deriving (Show, Eq)

eval :: MethodEnv -> GlobalEnv -> LocalEnv -> Exp -> IO WHNF
eval methods global local =
  \case
    LitE lit -> pure (LitW lit)
    LamE (LocalVarId param) body -> pure (LamW local param body)
    LetE binds body ->
      eval
        methods
        global
        (foldl'
           (\localEnv (LocalVarId i, ex) -> HM.insert i ex localEnv)
           local
           binds)
        body
    VarE var ->
      case var of
        ExportedIndex i ->
          case HM.lookup i global of
            Nothing -> error "eval.VarE.ExportedIndex = Nothing"
            Just e -> eval methods global local e
        LocalIndex i ->
          case HM.lookup i local of
            Nothing -> error "eval.VarE.LocalIndex = Nothing"
            Just e -> eval methods global local e
    AppE (MethodE (MethId i)) dict ->
      case HM.lookup i methods of
        Nothing -> error "AppE.MethodE.MethId = Nothing"
        Just idx ->
          case idx of
            0 -> eval methods global local dict
            _ -> do
              whnf <- eval methods global local dict
              case whnf of
                ConW _cid args ->
                  case args V.!? (fromIntegral idx - 1) of
                    Nothing ->
                      error "AppE.Method: Couldn't find method in dictionary!"
                    Just (Thunk locals' e) -> eval methods global locals' e
                _ -> error "AppE.Method: Expected class dictionary!"
    AppE func arg -> do
      result <- eval methods global local func
      case result of
        LamW env param body ->
          eval methods global (HM.insert param arg env) body
        LitW lit -> error ("eval.AppE.LitW: expected function: " ++ show lit)
        ConW cid args -> pure (ConW cid (args <> V.singleton (Thunk local arg)))
    -- To support pattern matching.
    CaseE {} -> error "eval.CaseE: undefined"
    ConE cid -> pure (ConW cid mempty)
    -- To support $ and other hacks by GHC.
    WiredInE {} -> error "eval.WiredInE: undefined"
    -- To support type classes (needed for numbers).
    MethodE {} ->
      error "eval.MethodE: should not be evaluated directly, but has been!"
    -- To support numeric operations (ints, chars, etc. ..).
    PrimOpE {} -> error "eval.PrimOpE: undefined"
    -- To support FFI calls (probably do this last):
    FFIE {} -> error "eval.FFIE: undefined"
