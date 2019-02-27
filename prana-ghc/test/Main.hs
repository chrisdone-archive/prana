{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Print STG in GHC 8.4.3.

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
                  { indexGlobals =
                      M.fromList
                        (zip
                           [ Name
                               { namePackage = "ghc-prim"
                               , nameModule = "GHC.Types"
                               , nameName = "krep$*"
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "ghc-prim"
                               , nameModule = "GHC.Types"
                               , nameName = "[]"
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "base"
                               , nameModule = "GHC.Num"
                               , nameName = "fromInteger"
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "ghc-prim"
                               , nameModule = "GHC.Classes"
                               , nameName = "=="
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "base"
                               , nameModule = "GHC.Num"
                               , nameName = "-"
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "base"
                               , nameModule = "GHC.Num"
                               , nameName = "+"
                               , nameUnique = Exported
                               }
                           ]
                           (map (GlobalVarId . negate) [1 ..]))
                  , indexLocals = mempty
                  , indexDataCons =
                      M.fromList
                        (zip
                           [ Name
                               { namePackage = "ghc-prim"
                               , nameModule = "GHC.Types"
                               , nameName = "TrNameS"
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "integer-gmp"
                               , nameModule = "GHC.Integer.Type"
                               , nameName = "S#"
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "ghc-prim"
                               , nameModule = "GHC.Types"
                               , nameName = "False"
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "ghc-prim"
                               , nameModule = "GHC.Types"
                               , nameName = "True"
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "ghc-prim"
                               , nameModule = "GHC.Types"
                               , nameName = "Module"
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "ghc-prim"
                               , nameModule = "GHC.Types"
                               , nameName = "TyCon"
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "ghc-prim"
                               , nameModule = "GHC.Types"
                               , nameName = "KindRepTyConApp"
                               , nameUnique = Exported
                               }
                           , Name
                               { namePackage = "ghc-prim"
                               , nameModule = "GHC.Types"
                               , nameName = "[]"
                               , nameUnique = Exported
                               }
                           ]
                           (map (DataConId . negate) [1 ..]))
                  })
           liftIO (print index')))
