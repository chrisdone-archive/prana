{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Frontend plugin for GHC.

module Prana.FrontendPlugin
  ( frontendPlugin
  , dumpCode
  ) where

import           Bag
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as L
import           Data.Data
import           Data.Generics
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified FastString as GHC
import qualified GHC
import qualified GhcPlugins
import qualified Id as GHC
import qualified Module as GHC
import qualified Name as GHC
import           Prana.Types

--------------------------------------------------------------------------------
-- Frontend

frontendPlugin :: GhcPlugins.FrontendPlugin
frontendPlugin = GhcPlugins.defaultFrontendPlugin {
  GhcPlugins.frontend = frontend
  }

frontend :: [String] -> [(String, Maybe GHC.Phase)] -> GHC.Ghc ()
frontend _flags args = do
  targets <- mapM (uncurry GHC.guessTarget) args
  GHC.setTargets targets
  _ <- GHC.load GHC.LoadAllTargets
  dumpCode id

--------------------------------------------------------------------------------
-- Compilation

-- | Type-check the module and track through it.
compile ::
     GHC.GhcMonad m
  => (GHC.Module -> GHC.HsExpr GHC.Id -> Set ByteString)
  -> GHC.ModSummary
  -> m [Binding]
compile shouldFlag modSummary = do
  df <- GHC.getSessionDynFlags
  parsedModule <- GHC.parseModule modSummary
  typecheckedModule <- GHC.typecheckModule parsedModule
  pure []

--------------------------------------------------------------------------------
-- Dumping code

-- | Go through the module graph.
dumpCode :: GHC.GhcMonad m => (Binding -> Binding) -> m ()
dumpCode f = do
  mgraph <- GHC.getModuleGraph
  mapM_
    (\modSummary -> do
       bs <- compile (const (const mempty)) modSummary
       liftIO
         (L.writeFile
            (moduleToFilePath (GHC.ms_mod modSummary))
            (L.toLazyByteString "")))
    mgraph

moduleToFilePath :: GHC.Module -> FilePath
moduleToFilePath module' = packageNameVersion ++ "_" ++ moduleNameString ++ ".prana"
  where
    unitId = GHC.moduleUnitId module'
    moduleName = GHC.moduleName module'
    packageNameVersion = GHC.unitIdString unitId
    moduleNameString = GHC.moduleNameString moduleName

--------------------------------------------------------------------------------
-- JSON writing

array :: [L.Builder] -> L.Builder
array xs = "[" <> mconcat (intersperse "\n," xs) <> "]"

object :: [(ByteString, L.Builder)] -> L.Builder
object keys =
  "{" <>
  mconcat
    (intersperse
       "\n,"
       (map (\(k, v) -> string k <> ": " <> v) keys)) <>
  "}"

int :: Int -> L.Builder
int s = L.byteString (S8.pack (show s))

string :: ByteString -> L.Builder
string s = L.byteString (S8.pack (show s))
