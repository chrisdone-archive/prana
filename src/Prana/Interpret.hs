{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Prana.Interpret where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Prana.Types

-- | An environment to evaluate expressions in.
data Env = Env
  { envScope :: HashMap ByteString Thunk
  }

-- | A thunk which may be forced or not.
data Thunk = Thunk
  { thunkExp :: Exp
  , thunkValue :: IORef (Maybe Exp)
  }

-- | Evaluation computation.
newtype Eval a = Eval
  { runEval :: ReaderT Env IO a
  } deriving (MonadIO, Monad, Applicative, Functor)

-- | Interpret the expression.
interpret :: Exp -> Eval Exp
interpret = undefined

-- | Read the thunk, forcing it if not already.
forceThunk :: Thunk -> Eval Exp
forceThunk Thunk {..} = do
  mvalue <- liftIO (readIORef thunkValue)
  case mvalue of
    Nothing -> do
      value <- interpret thunkExp
      liftIO (atomicWriteIORef thunkValue (Just value))
      pure value
    Just value -> pure value
