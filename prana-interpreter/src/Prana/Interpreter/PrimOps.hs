{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Primitive operations implementations.

module Prana.Interpreter.PrimOps
  ( evalPrimOp
  ) where

import GHC.Exts
import Prana.Interpreter.PrimOps.TH
import Prana.Interpreter.Types
import Prana.Types

evalPrimOp ::
     ReverseIndex
  -> (SomeVarId -> IO Whnf)
  -> PrimOp
  -> [Arg]
  -> PrimOpType
  -> IO Whnf
evalPrimOp index evalSomeVarId primOp args typ =
  $(derivePrimOpsCase
      Options
        { optionsOp = 'primOp
        , optionsArgs = 'args
        , optionsEvalInt = 'evalIntArg
        , optionsEvalSomeVarId = 'evalSomeVarId
        , optionsManualImplementations = [('TagToEnumOp, 'tagToEnum)]
        , optionsType = 'typ
        , optionsIndex = 'index
        })

evalIntArg :: (SomeVarId -> IO Whnf) -> Arg -> IO Int
evalIntArg evalSomeVarId =
  \case
    LitArg (IntLit !i) -> pure i
    LitArg lit -> error ("Invalid lit rep: " ++ show lit)
    VarArg someVarId -> do
      whnf <- evalSomeVarId someVarId
      case whnf of
        LitWhnf (IntLit !i) -> pure i
        LitWhnf lit -> error ("Invalid lit rep: " ++ show lit)
        _ ->
          error
            ("Unexpected whnf for evalIntArg (I'm sure ClosureWhnf will come up here): " ++
             show whnf)

tagToEnum :: ReverseIndex -> PrimOpType -> (SomeVarId -> IO Whnf) -> [Arg] -> IO Whnf
tagToEnum index typ evalSomeVarId args =
  case args of
    [arg] -> do
      (I# ii) <- evalIntArg evalSomeVarId arg
      case typ of
        BoolType -> do
          let bool = tagToEnum# ii :: Bool
              !con =
                case bool of
                  False -> reverseIndexFalse index
                  True -> reverseIndexTrue index
          pure (ConWhnf con [])
        _ -> error "Unknown type for tagToEnum."
    _ -> error ("Invalid arguments to TagToEnumOp: " ++ show args)
