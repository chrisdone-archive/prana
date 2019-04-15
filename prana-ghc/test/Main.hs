{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Print STG in GHC 8.4.3.

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import qualified Data.ByteString.Builder as L
import qualified DynFlags
import qualified GHC
import qualified GHC.Paths
import           Prana.Ghc
import           Prana.Index
import           Prana.Rename
import           Prana.Types

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
           options <- liftIO getOptions
           result <- compileModuleGraph options
           case result of
             Right (index, bindings) -> do
               let name =
                     Name
                       { namePackage = "main"
                       , nameModule = "Fib"
                       , nameName = "it"
                       , nameUnique = Exported
                       }
               liftIO
                 (case lookupGlobalBindingRhsByName index bindings name of
                    Just (RhsClosure closure@Closure{closureParams = []}) -> do
                      -- ghcPrim <- loadLibrary options "ghc-prim"
                      -- integerGmp <- loadLibrary options "integer-gmp"
                      -- base <- loadLibrary options "base"
                      print closure
                      print (lookupGlobalBindingRhsById bindings (GlobalVarId 56634))
                      {-interpret bindings expr-}
                    Just _ -> putStrLn "The expression should take no arguments."
                    Nothing -> putStrLn ("Couldn't find " <> displayName name))
             Left err -> showErrors err))

interpret :: Expr -> IO ()
interpret = go
  where go = \case
                _ -> undefined

displayExpr :: Expr -> Reader Index L.Builder
displayExpr = undefined
