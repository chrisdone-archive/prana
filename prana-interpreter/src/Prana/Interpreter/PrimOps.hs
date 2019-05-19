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

import Data.Word
import GHC.Exts
import GHC.Int
import Prana.Interpreter.Boxing
import Prana.Interpreter.PrimOps.TH
import Prana.Interpreter.Types
import Prana.Types

--------------------------------------------------------------------------------
-- Derived primops

evalPrimOp ::
     ReverseIndex
  -> (SomeVarId -> IO Whnf)
  -> PrimOp
  -> [Arg]
  -> Maybe TypeId
  -> IO Whnf
evalPrimOp index evalSomeVarId primOp args mtyp =
  $(derivePrimOpsCase
      Options
        { optionsOp = 'primOp
        , optionsArgs = 'args
        , optionsEvalSomeVarId = 'evalSomeVarId
        , optionsManualImplementations = [('TagToEnumOp, 'tagToEnum)]
        , optionsType = 'mtyp
        , optionsIndex = 'index
        , optionsEvalInt = 'evalIntArg
        , optionsBoxInt = 'boxInt
        , optionsEvalChar = 'evalCharArg
        , optionsBoxChar = 'boxChar
        , optionsEvalDouble = 'evalDoubleArg
        , optionsBoxDouble = 'boxDouble
        , optionsEvalFloat = 'evalFloatArg
        , optionsBoxFloat = 'boxFloat
        , optionsEvalWord = 'evalWordArg
        , optionsBoxWord = 'boxWord
        })

--------------------------------------------------------------------------------
-- Special primops with custom implementations

tagToEnum :: ReverseIndex -> Maybe TypeId -> (SomeVarId -> IO Whnf) -> [Arg] -> IO Whnf
tagToEnum _index mtypeId evalSomeVarId args =
  case mtypeId of
    Nothing -> error "TagToEnumOp not given a type!"
    Just typeId ->
      case args of
        [arg] -> do
          ii <- evalIntArg evalSomeVarId arg
          pure (ConWhnf (DataConId typeId (ConIndex (fromIntegral ii))) [])
        _ -> error ("Invalid arguments to TagToEnumOp: " ++ show args)

--------------------------------------------------------------------------------
-- Evaluating arguments for primops

evalIntArg :: (SomeVarId -> IO Whnf) -> Arg -> IO Int
evalIntArg evalSomeVarId = $(evalArgByType 'evalSomeVarId 'IntLit)

evalCharArg :: (SomeVarId -> IO Whnf) -> Arg -> IO Char
evalCharArg evalSomeVarId = $(evalArgByType 'evalSomeVarId 'CharLit)

evalFloatArg :: (SomeVarId -> IO Whnf) -> Arg -> IO Float
evalFloatArg evalSomeVarId = $(evalArgByType 'evalSomeVarId 'FloatLit)

evalDoubleArg :: (SomeVarId -> IO Whnf) -> Arg -> IO Double
evalDoubleArg evalSomeVarId = $(evalArgByType 'evalSomeVarId 'DoubleLit)

evalWordArg :: (SomeVarId -> IO Whnf) -> Arg -> IO Word
evalWordArg evalSomeVarId = $(evalArgByType 'evalSomeVarId 'WordLit)
