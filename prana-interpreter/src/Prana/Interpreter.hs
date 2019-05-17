module Prana.Interpreter
  (
  -- * Setting up the environment
    bindGlobal
  -- * Evaluation functions
  , evalExpr
  , evalCon
  , evalBox
  -- * Data types
  , Whnf(..)
  , Thunk(..)
  , Box(..)
  ) where

import Prana.Interpreter.Binding
import Prana.Interpreter.Eval
import Prana.Interpreter.Types
