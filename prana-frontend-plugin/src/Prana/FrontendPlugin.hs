{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}
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
import           CoreSyn
import           Data.Binary.Builder
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as L
import qualified Data.ByteString.Lazy.Char8 as L8
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
import           HIndent
import           HIndent.Types
import qualified HscTypes as GHC
import qualified Id as GHC
import qualified Literal as GHC
import qualified Module as GHC
import qualified Name as GHC
import qualified Outputable as GHC
import           Prana.Types
import           TcRnTypes


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
  parsedModule <- GHC.parseModule modSummary
  typecheckedModule <- GHC.typecheckModule parsedModule
  desugared <- GHC.desugarModule typecheckedModule
  df <- GHC.getSessionDynFlags
  let binds = GHC.mg_binds (GHC.dm_core_module desugared)
  liftIO
    (L.putStrLn
       (either
          L8.pack
          toLazyByteString
          (reformat
             defaultConfig
             Nothing
             (S8.pack
                ("[" ++ intercalate ", " (map (showBind False) binds) ++ "]")))))
  pure []

--------------------------------------------------------------------------------
-- Printing

showBind :: Bool -> CoreSyn.Bind GHC.Var -> String
showBind ps =
  \case
    CoreSyn.NonRec var expr ->
      parens ps ("NonRec " ++ showVar True var ++ " " ++ showExpr True expr)
    CoreSyn.Rec exprs ->
      parens
        ps
        ("Rec [" ++
         intercalate
           ","
           (map
              (\(x, y) ->
                 "(" ++ showVar False x ++ ", " ++ showExpr False y ++ ")")
              exprs) ++
         "]")

showVar :: Bool -> GHC.Var -> String
showVar ps v = parens ps ("Var " ++ show (GHC.nameStableString (GHC.getName v)))

showId :: Bool -> GHC.Id -> String
showId ps v = parens ps ("Id " ++ show (GHC.nameStableString (GHC.getName v)))

showExpr :: Bool -> CoreSyn.Expr GHC.Var -> String
showExpr ps =
  \case
    CoreSyn.Var vid -> parens ps ("Var " ++ showId ps vid)
    CoreSyn.Lit literal -> parens ps ("Lit "++showLiteral True literal)
    CoreSyn.App f x ->
      parens ps ("App " ++ showExpr True f ++ " " ++ showExpr True x)
    CoreSyn.Lam var body -> "Lam"
    CoreSyn.Let bind expr -> "Let"
    CoreSyn.Case expr var typ alts -> "Case"
    CoreSyn.Cast expr coercion -> "Cast"
    CoreSyn.Tick tickishVar expr -> "Tick"
    CoreSyn.Type typ -> "Type"
    CoreSyn.Coercion coercion -> "Coercion"

showLiteral :: Bool -> GHC.Literal -> String
showLiteral ps =
  \case
    GHC.MachChar ch -> "MachChar"
    GHC.MachStr str -> "MachStr"
    GHC.MachNullAddr -> "MachNullAddr"
    GHC.MachInt i -> "MachInt"
    GHC.MachInt64 i -> "MachInt64"
    GHC.MachWord i -> "MachWord"
    GHC.MachWord64 i -> "MachWord64"
    GHC.MachFloat r -> "MachFloat"
    GHC.MachDouble r -> "MachDouble"
    GHC.MachLabel fastString mint functionOrData -> "MachLabel"
    GHC.LitInteger i typ -> "LitInteger"

parens True x = "(" ++ x ++ ")"
parens _    x = x

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
