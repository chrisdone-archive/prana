{-# LANGUAGE RecursiveDo #-}

-- | Binding names in the environment.

module Prana.Interpreter.Binding
  ( bindLocal
  , bindGlobal
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Prana.Interpreter.Boxing
import           Prana.Interpreter.Types
import           Prana.Types

bindLocal :: LocalBinding -> Map LocalVarId Box -> IO (Map LocalVarId Box)
bindLocal localBinding locals =
  case localBinding of
    LocalNonRec var rhs -> do
      box <- boxRhs locals rhs
      let locals' = M.insert var box locals
      pure locals'
    LocalRec pairs -> mdo
      locals' <-
        foldM
          (\acc (var, rhs) -> do
             box <- boxRhs locals' rhs
             pure (M.insert var box acc))
          locals
          pairs
      pure locals'

bindGlobal ::
     MonadIO m
  => Map GlobalVarId Box
  -> GlobalBinding
  -> m (Map GlobalVarId Box)
bindGlobal globals globalBinding =
  liftIO
    (case globalBinding of
       GlobalNonRec var rhs -> do
         box <- boxRhs mempty rhs
         let globals' = M.insert var box globals
         pure globals'
       GlobalRec pairs -> do
         globals' <-
           foldM
             (\acc (var, rhs) -> do
                box <- boxRhs mempty rhs
                pure (M.insert var box acc))
             globals
             pairs
         pure globals'
       GlobalStringLit globalVarId byteString -> do
         box <- boxWhnf (LitWhnf (StringLit byteString))
         pure (M.insert globalVarId box globals))
