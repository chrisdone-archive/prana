{-# LANGUAGE LambdaCase #-}

-- |

module Prana.Interpreter.Boxing where

import           Data.IORef
import           Data.Map.Strict (Map)
import           Prana.Interpreter.Types
import           Prana.Types

boxArg :: Map LocalVarId Box -> Arg -> IO Box
boxArg locals =
  \case
    VarArg someVarId -> fmap Box (newIORef (VariableThunk locals someVarId))
    LitArg lit -> fmap Box (newIORef (WhnfThunk (LitWhnf lit)))

boxRhs :: Map LocalVarId Box -> Rhs -> IO Box
boxRhs locals =
  \case
    RhsCon (Con dataConId args) -> do
      boxes <- traverse (boxArg locals) args
      fmap Box (newIORef (WhnfThunk (ConWhnf dataConId boxes)))
    RhsClosure closure ->
      if null (closureParams closure)
        then fmap Box (newIORef (ExpressionThunk locals (closureExpr closure)))
        else fmap
               Box
               (newIORef
                  (WhnfThunk
                     (FunWhnf
                        locals
                        (closureParams closure)
                        (closureExpr closure))))

boxWhnf :: Whnf -> IO Box
boxWhnf whnf =
  fmap Box (newIORef (WhnfThunk whnf))
