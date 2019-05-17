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
  case primOp of

    -- Int# -> Int# -> (# Int#, Int# #)
    IntSubCOp ->
      case args of
        [arg1, arg2] -> do
          I# i <- evalIntArg evalSomeVarId arg1
          I# i2 <- evalIntArg evalSomeVarId arg2
          case subIntC# i i2 of
            (# x#, y# #) -> do
              xBox <- boxWhnf (LitWhnf (IntLit (I# x#)))
              yBox <- boxWhnf (LitWhnf (IntLit (I# y#)))
              pure (ConWhnf (UnboxedTupleConId 2) [xBox, yBox])
        _ -> error ("Invalid arguments to IntSubCOp: " ++ show args)
    -- IntEqOp -> $(op [| (==#) :: Int# -> Int# -> Int# |])
    -- IntLtOp -> $(op [| (<#) :: Int# -> Int# -> Int# |])
    -- IntAddOp -> $(op [| (+#) :: Int# -> Int# -> Int# |])
    -- IntSubOp -> $(op [| (-#) :: Int# -> Int# -> Int# |])
    -- IntNegOp -> $(op [| negateInt# :: Int# -> Int# |])
    TagToEnumOp ->
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

    _ ->
      error
        ("Unimplemented primop: " ++
         show primOp ++ " (type: " ++ show typ ++ "), args were: " ++ show args)

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
