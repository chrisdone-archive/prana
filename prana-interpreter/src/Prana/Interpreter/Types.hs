
-- | Types used by the interpreter.

module Prana.Interpreter.Types where

import Data.IORef
import Data.Map.Strict (Map)
import GHC.Exts
import Prana.Types

-- | A runtime value at WHNF.
data Whnf
  = LitWhnf Lit
  | AddrWhnf (Ptr ())
  | ConWhnf DataConId [Box]
  | FunWhnf (Map LocalVarId Box) [LocalVarId] Expr
  | EmptyWhnf
  deriving (Show, Eq)

-- | Provides laziness: A boxed value which is not necessarily
-- evaluated, and when it is evaluated, the thunk inside it is
-- updated.
newtype Box = Box { boxIORef :: IORef Thunk } deriving (Eq)
instance Show Box where show _ = "Box"

-- | A thunk which has either been evaluated to WHNF, or references a
-- variable or expression to force on demand.
data Thunk
  = VariableThunk (Map LocalVarId Box) SomeVarId
  | ExpressionThunk (Map LocalVarId Box) Expr
  | WhnfThunk Whnf
  deriving (Show, Eq)
