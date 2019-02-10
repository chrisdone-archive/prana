-- |

module Main where

import qualified GHC
import qualified HscTypes as GHC
import Test.Hspec

main :: IO ()
main = pure ()

compile ::
     GHC.GhcMonad m
  => GHC.ModSummary
  -> m GHC.ModGuts
compile modSummary = do
  parsedModule <- GHC.parseModule modSummary
  typecheckedModule <- GHC.typecheckModule parsedModule
  desugared <- GHC.desugarModule typecheckedModule
  pure (GHC.dm_core_module desugared)
