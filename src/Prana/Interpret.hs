{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- |

module Prana.Interpret where

import           Control.Concurrent
import           Control.Exception (Exception, throw)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.Typeable
import           GHC.Base
import           GHC.Prim
import           Prana.Types

-- | An environment to evaluate expressions in.
newtype Env = Env
  { envGlobals :: IORef (HashMap Id Exp)
  }

-- | Evaluation computation.
newtype Eval a = Eval
  { runEval :: ReaderT Env IO a
  } deriving (MonadIO, Monad, Applicative, Functor)

-- | A interpreter error in the interpreter.
data InterpreterError =
  TypeError TypeError
  deriving (Show, Typeable)
instance Exception InterpreterError

-- | A type error in the interpreter.
data TypeError =
  NotAFunction WHNF
  deriving (Show, Typeable)

-- | An expression evaluated to weak head normal form.
data WHNF
  = OpWHNF Id
  | PrimWHNF Prim
  | ConWHNF Id [Exp]
  | LamWHNF Var Exp
  deriving (Show)

-- | A primitive value.
data Prim
  = CharPrim Char
  | AddrPrim Addr
  | FloatPrim Float
  | DoublePrim Double
  | IntPrim Int
  | WordPrim Word
  | ThreadId ThreadId
  deriving (Show)

-- | Some address from GHC.Prim.
data Addr = Addr Addr#
instance Show Addr where
  show (Addr a) = show (I# (addr2Int# a))

-- | Interpret the expression.
whnf :: Exp -> Eval WHNF
whnf =
  \case
    LitE l -> pure undefined
    VarE l -> pure undefined
    AppE f arg -> do
      result <- whnf f
      case result of
        LamWHNF v body -> undefined -- TODO: beta-substitute!
        OpWHNF id -> undefined -- TODO: run the primop!
        _ -> throw (TypeError (NotAFunction result))
    LamE i e -> pure (LamWHNF i e)
    CaseE {} -> undefined

resolveGlobal :: Id -> Eval Exp
resolveGlobal = undefined
