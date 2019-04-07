{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Print STG in GHC 8.4.3.

module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified DynFlags
import qualified GHC
import qualified GHC.Paths
import           Prana.Ghc

main :: IO ()
main =
  GHC.defaultErrorHandler
    DynFlags.defaultFatalMessager
    DynFlags.defaultFlushOut
    (GHC.runGhc
       (Just GHC.Paths.libdir)
       (do dflags <- GHC.getSessionDynFlags
           _ <- GHC.setSessionDynFlags dflags
           target <- GHC.guessTarget "Fib.hs" Nothing
           GHC.setTargets [target]
           _ <- GHC.load GHC.LoadAllTargets
           compileModuleGraphFromEnv
           liftIO (putStrLn "Good to go!")))
