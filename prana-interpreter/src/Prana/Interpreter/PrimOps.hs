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

-- TODO: Generalize these two with to e.g. evalPrimArg.

-- Perhaps using e.g. evalToLit :: Arg -> IO Lit; and then pattern matching off of the Lit.

-- Others to consider: Double#, Word#, Float#, Addr#

evalIntArg :: (SomeVarId -> IO Whnf) -> Arg -> IO Int
evalIntArg evalSomeVarId = $(evalArgByType 'evalSomeVarId 'IntLit)

evalCharArg :: (SomeVarId -> IO Whnf) -> Arg -> IO Char
evalCharArg evalSomeVarId = $(evalArgByType 'evalSomeVarId 'CharLit)
