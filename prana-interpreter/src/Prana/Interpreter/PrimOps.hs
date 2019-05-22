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

import Data.Map.Strict (Map)
import Data.Primitive
import Data.Primitive.SmallArray
import Foreign.Ptr
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
  -> (Box -> IO Whnf)
  -> Map LocalVarId Box
  -> PrimOp
  -> [Arg]
  -> Maybe TypeId
  -> IO Whnf
evalPrimOp index evalSomeVarId evalBox locals primOp args mtyp =
  $(derivePrimOpsCase
      Options
        { optionsOp = 'primOp
        , optionsLocals = 'locals
        , optionsArgs = 'args
        , optionsEvalSomeVarId = 'evalSomeVarId
        , optionsEvalBox = 'evalBox
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
        , optionsEvalAddr = 'evalAddrArg
        , optionsBoxAddr = 'boxAddr
        , optionsEvalArray = 'evalArrayArg
        , optionsBoxArray = 'boxArray
        , optionsEvalMutableArray = 'evalMutableArrayArg
        , optionsBoxMutableArray = 'boxMutableArray
        , optionsEvalSmallMutableArray = 'evalSmallMutableArrayArg
        , optionsBoxSmallMutableArray = 'boxSmallMutableArray
        , optionsEvalMutableByteArray = 'evalMutableByteArrayArg
        , optionsBoxMutableByteArray = 'boxMutableByteArray
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

evalAddrArg :: (SomeVarId -> IO Whnf) -> Arg -> IO (Ptr ())
evalAddrArg evalSomeVarId =
  \case
    LitArg NullAddrLit -> pure nullPtr
    VarArg someVarId -> do
      whnf <- evalSomeVarId someVarId
      case whnf of
        LitWhnf NullAddrLit -> pure nullPtr
        AddrWhnf ptr -> pure ptr
        _ -> error ("Unexpected addr evaluating an addr...")
    _ -> error ("Unexpected addr evaluating an addr...")

evalArrayArg :: (SomeVarId -> IO Whnf) -> Arg -> IO (Array Box)
evalArrayArg evalSomeVarId =
  \case
    VarArg someVarId -> do
      whnf <- evalSomeVarId someVarId
      case whnf of
        ArrayWhnf ptr -> pure ptr
        _ -> error ("Unexpected array evaluating an array...")
    _ -> error ("Unexpected array evaluating an array...")

evalMutableArrayArg :: (SomeVarId -> IO Whnf) -> Arg -> IO (MutableArray RealWorld Box)
evalMutableArrayArg evalSomeVarId =
  \case
    VarArg someVarId -> do
      whnf <- evalSomeVarId someVarId
      case whnf of
        MutableArrayWhnf (MutableRealWorldArray ptr) -> pure ptr
        _ -> error ("Unexpected MutableArray evaluating an MutableArray...")
    _ -> error ("Unexpected MutableArray evaluating an MutableArray...")

evalMutableByteArrayArg :: (SomeVarId -> IO Whnf) -> Arg -> IO (MutableByteArray RealWorld)
evalMutableByteArrayArg evalSomeVarId =
  \case
    VarArg someVarId -> do
      whnf <- evalSomeVarId someVarId
      case whnf of
        MutableByteArrayWhnf (MutableRealWorldByteArray ptr) -> pure ptr
        _ -> error ("Unexpected MutableByteArray evaluating an MutableByteArray...")
    _ -> error ("Unexpected MutableByteArray evaluating an MutableByteArray...")

evalSmallMutableArrayArg :: (SomeVarId -> IO Whnf) -> Arg -> IO (SmallMutableArray RealWorld Box)
evalSmallMutableArrayArg evalSomeVarId =
  \case
    VarArg someVarId -> do
      whnf <- evalSomeVarId someVarId
      case whnf of
        SmallMutableArrayWhnf (SmallMutableRealWorldArray ptr) -> pure ptr
        _ -> error ("Unexpected SmallMutableArray evaluating an SmallMutableArray...")
    _ -> error ("Unexpected SmallMutableArray evaluating an SmallMutableArray...")
