-- | Harness for running the GHC compiler.

module GHCHarness where

import GHC
import GHC.Paths ( libdir )
import DynFlags

compileWith :: [FilePath] -> Ghc a -> IO a
compileWith fps m =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      _ <- setSessionDynFlags dflags
      targets <- mapM (\fp -> guessTarget fp Nothing) fps
      setTargets targets
      _ <- load LoadAllTargets
      m
