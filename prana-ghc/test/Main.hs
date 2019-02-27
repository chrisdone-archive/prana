{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Print STG in GHC 8.4.3.

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State
import qualified DynFlags
import qualified GHC
import qualified GHC.Paths
import           Prana.Ghc
import           Prana.Index

main :: IO ()
main =
  GHC.defaultErrorHandler
    DynFlags.defaultFatalMessager
    DynFlags.defaultFlushOut
    (GHC.runGhc
       (Just GHC.Paths.libdir)
       (do dflags <- GHC.getSessionDynFlags
           _ <- GHC.setSessionDynFlags dflags
           target <- GHC.guessTarget "stgdemo.hs" Nothing
           GHC.setTargets [target]
           _ <- GHC.load GHC.LoadAllTargets
           mgraph <- fmap GHC.mgModSummaries GHC.getModuleGraph
           index' <-
             execStateT
               (mapM_
                  (\modSummary -> do
                     result <- compileModSummary modSummary
                     case result of
                       Left compileError -> liftIO (print compileError)
                       Right bindings -> liftIO (print bindings))
                  mgraph)
               (Index
                  { indexGlobals = mempty
                  , indexLocals = mempty
                  , indexDataCons = mempty
                  })
           pure ()))
