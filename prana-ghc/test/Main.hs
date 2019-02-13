-- |

module Main where

import qualified GHC
import qualified StgSyn as GHC

main :: IO ()
main = pure ()

compile ::
     GHC.GhcMonad m
  => GHC.ModSummary
  -> m [GHC.StgTopBinding]
compile modSummary = do
  parsedModule <- GHC.parseModule modSummary
  typecheckedModule <- GHC.typecheckModule parsedModule
  desugared <- GHC.desugarModule typecheckedModule
  pure (GHC.dm_core_module desugared)
  undefined
