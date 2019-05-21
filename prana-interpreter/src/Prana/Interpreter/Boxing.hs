{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

-- |

module Prana.Interpreter.Boxing where

import Data.IORef
import Data.Map.Strict (Map)
import Data.Primitive
import GHC.Exts
import Prana.Interpreter.Types
import Prana.Types

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

boxInt :: Int# -> IO Box
boxInt x# = boxWhnf (LitWhnf (IntLit (I# x#)))

boxChar :: Char# -> IO Box
boxChar x# = boxWhnf (LitWhnf (CharLit (C# x#)))

boxWord :: Word# -> IO Box
boxWord x# = boxWhnf (LitWhnf (WordLit (W# x#)))

boxDouble :: Double# -> IO Box
boxDouble x# = boxWhnf (LitWhnf (DoubleLit (D# x#)))

boxFloat :: Float# -> IO Box
boxFloat x# = boxWhnf (LitWhnf (FloatLit (F# x#)))

boxAddr :: Addr# -> IO Box
boxAddr x# = boxWhnf (AddrWhnf (Ptr x#))

boxArray :: Array# Box -> IO Box
boxArray x# = boxWhnf (ArrayWhnf (Array x#))
