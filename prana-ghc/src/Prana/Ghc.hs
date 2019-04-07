{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Import from GHC into Prana.
--
-- Pipeline:
--
-- 0. COMPILE each module.
--
-- 1. RENAME each module.
--
-- 2. COLLECT each module (get data cons, get globals, get locals).
--
-- 3. RECONSTRUCT each module.

module Prana.Ghc
  ( compileModuleGraphFromEnv
  , compileModSummary
  , compileModuleGraph
  , getOptions
  , showErrors
  , lookupGlobalBindingRhs
  ) where

import           Control.Exception
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified CorePrep
import qualified CoreSyn
import qualified CoreToStg
import qualified CostCentre
import           Data.Binary (encode, decode, Binary)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Either
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Validation
import qualified Digraph
import qualified DynFlags
import qualified FastString
import qualified GHC
import           HscTypes
import qualified Module
import qualified Outputable
import           Prana.Collect
import           Prana.Index
import           Prana.Reconstruct
import           Prana.Rename
import           Prana.Types
import qualified SimplStg
import qualified StgSyn as GHC
import           System.Directory
import           System.Environment
import           TidyPgm
import qualified TyCon

--------------------------------------------------------------------------------
-- Types

data CompileError
  = RenameErrors (NonEmpty RenameFailure)
  | ConvertErrors (NonEmpty ConvertError)
  deriving (Show)

data Mode = DEV | INSTALL
  deriving (Show, Enum, Bounded)

data Options =
  Options
    { optionsDir :: FilePath
    , optionsMode :: Mode
    }

--------------------------------------------------------------------------------
-- Parse options for this compiler mod

getOptions :: IO Options
getOptions =
  Options <$> getEnv "PRANA_DIR" <*>
  (lookupEnv "PRANA_MODE" >>=
   (\case
      Just "DEV" -> pure DEV
      Just "INSTALL" -> pure INSTALL
      _ ->
        error
          ("Please specify a mode via PRANA_MODE: " ++
           show [minBound .. maxBound :: Mode])))

optionsIndexPath :: Options -> FilePath
optionsIndexPath options = optionsDir options <> "/index"

optionsIndexTmpPath :: Options -> FilePath
optionsIndexTmpPath options = optionsIndexTmpPath options <> ".tmp"

optionsPackagesDir :: Options -> FilePath
optionsPackagesDir options = optionsDir options <> "/packages/"

--------------------------------------------------------------------------------
-- Entry point for modded GHC

-- | Read in arguments from PRANA_ARGS
compileModuleGraphFromEnv :: GHC.Ghc ()
compileModuleGraphFromEnv = do
  options <- liftIO getOptions
  void (compileModuleGraph options)

--------------------------------------------------------------------------------
-- General entry point

-- | Compile all modules in the graph.
--
-- 1) Load up the names index.
-- 2) Compile each module in the graph.
-- 3) Write back out the names index.
-- 4) Write out the files.
compileModuleGraph ::
     Options
  -> GHC.Ghc (Either (Map Module.Module CompileError) (Index, [GlobalBinding]))
compileModuleGraph options = do
  liftIO (createDirectoryIfMissing True (optionsPackagesDir options))
  dflags <- GHC.getSessionDynFlags
  GHC.setSessionDynFlags dflags {DynFlags.hscTarget = DynFlags.HscAsm}
  mgraph <-
    fmap (\g -> GHC.topSortModuleGraph False g Nothing) GHC.getModuleGraph
  index0 <- liftIO (readIndex (optionsIndexPath options))
  ((listOfListOfbindings, errors), index') <-
    (runStateT (runWriterT (buildGraph mgraph)) index0)
  if M.null errors
    then do
      let allBindings = concat (rights listOfListOfbindings)
      case optionsMode options of
        INSTALL -> installPackage options index' allBindings
        DEV -> pure ()
      pure (Right (index', allBindings))
    else pure (Left errors)

-- | Install package by updating the index and writing the library
-- to a file in the packages dir.
installPackage ::
     (GHC.GhcMonad m, Binary binding)
  => Options
  -> Index
  -> [binding]
  -> m ()
installPackage options index' bindings = do
  dflags <- GHC.getSessionDynFlags
  let fp = pkg ++ ".prana"
      pkg =
        FastString.unpackFS
          (Module.installedUnitIdFS
             (Module.toInstalledUnitId (DynFlags.thisPackage dflags)))
      path = optionsPackagesDir options ++ "/" ++ fp
      pathTmp = optionsPackagesDir options ++ "/" ++ fp ++ ".tmp"
  liftIO
    (do S8.putStrLn "Updating index ..."
        L.writeFile (optionsIndexTmpPath options) (encode (index' :: Index)))
  liftIO
    (do S8.putStrLn (S8.pack ("Writing library " ++ pkg ++ " ..."))
        L.writeFile pathTmp (encode bindings))
  liftIO
    (do renameFile (optionsIndexTmpPath options) (optionsIndexPath options)
        renameFile pathTmp path)

-- | Build the graph, writing errors, if any, and returning either error or success.
buildGraph ::
     [Digraph.SCC ModSummary]
  -> WriterT (Map Module.Module CompileError)
             (StateT Index GHC.Ghc)
             [Either CompileError [GlobalBinding]]
buildGraph mgraph = do
  let sccs = Digraph.flattenSCCs mgraph
      total = length sccs
  modules <-
    mapM
      (\(i, modSummary) ->
         fmap (GHC.ms_mod modSummary, ) (compileToPrana total i modSummary))
      (zip [1 :: Int ..] sccs)
  mapM
    (\(i, (module', result)) -> do
       let modName = Outputable.showSDocUnsafe (Outputable.ppr module')
       liftIO
         (S8.putStrLn
            (S8.pack
               ("[" <> show i <> " of " <> show total <> "] Converting " <>
                modName)))
       index' <- lift get
       let scope = Scope {scopeIndex = index', scopeModule = module'}
       case result of
         Left e -> pure (Left e)
         Right bindings ->
           case runReaderT
                  (runConvert (traverse fromGenStgTopBinding bindings))
                  scope of
             Failure errs -> do
               showErrors (M.singleton module' (ConvertErrors errs))
               pure (Left (ConvertErrors errs))
             Success globals -> pure (Right globals))
    (zip [1 :: Int ..] modules)

-- | Compile the module to a prana file and update the index.
compileToPrana ::
     Int
  -> Int
  -> ModSummary
  -> WriterT (Map GHC.Module CompileError)
             (StateT Index GHC.Ghc)
             (Either CompileError
                    [GHC.GenStgTopBinding Name Name])
compileToPrana total i modSummary = do
  let modName =
        Outputable.showSDocUnsafe (Outputable.ppr mn)
      mn = GHC.ms_mod modSummary
  liftIO
    (S8.putStrLn
          (S8.pack ("[" <> show i <> " of " <> show total <> "] Compiling " <> modName)))
  lift (compileModSummary modSummary)

-- | Compile the module summary to a set of global bindings, updating
-- the names index too.
--
-- I _believe_ this is the correct way to get STG from a module,
-- however I asked on the ghc-devs mailing list and got no reply:
-- <https://mail.haskell.org/pipermail/ghc-devs/2019-February/017117.html>
-- So it's really just guess-work here.
compileModSummary ::
     GHC.ModSummary
  -> StateT Index GHC.Ghc (Either CompileError [GHC.GenStgTopBinding Name Name])
compileModSummary modSum = do
  pmod <- lift (GHC.parseModule modSum)
  tmod <- lift (GHC.typecheckModule pmod)
  dmod <- lift (GHC.desugarModule tmod)
  let core = GHC.coreModule dmod
  hsc_env <- lift GHC.getSession
  let this_mod = GHC.ms_mod modSum
  (CgGuts {cg_binds = core_binds, cg_tycons = tycons}, _) <-
    liftIO (TidyPgm.tidyProgram hsc_env core)
  let data_tycons = filter TyCon.isDataTyCon tycons
  (prepd_binds, _) <-
    liftIO
      (CorePrep.corePrepPgm
         hsc_env
         this_mod
         (GHC.ms_location modSum)
         core_binds
         data_tycons)
  dflags <- lift DynFlags.getDynFlags
  (stg_binds, _) <- liftIO (myCoreToStg dflags this_mod prepd_binds)
  let module' = GHC.ms_mod modSum
      modguts = GHC.dm_core_module dmod
      tyCons = collectDataCons (HscTypes.mg_tcs modguts)
  case (,) <$> traverse (renameTopBinding module') stg_binds <*>
       traverse (validationNel . renameId module') (Set.toList tyCons) of
    Failure errors -> pure (Left (RenameErrors errors))
    Success (bindings, dataCons) -> do
      void (updateIndex bindings dataCons)
      pure (Right bindings)

-- | Perform core to STG transformation.
myCoreToStg ::
     GHC.DynFlags
  -> GHC.Module
  -> CoreSyn.CoreProgram
  -> IO ([GHC.StgTopBinding], CostCentre.CollectedCCs)
myCoreToStg dflags this_mod prepd_binds = do
  let (stg_binds, cost_centre_info) = CoreToStg.coreToStg dflags this_mod prepd_binds
  stg_binds2 <- SimplStg.stg2stg dflags stg_binds
  return (stg_binds2, cost_centre_info)

-- | Show errors.
showErrors ::
     (MonadIO m) => Map GHC.Module CompileError -> m ()
showErrors errors =
  liftIO
    (mapM_
       (\(modName, compileErrors) -> do
          S8.putStrLn
            (S8.pack
               ("\nErrors in " ++
                (Outputable.showSDocUnsafe (Outputable.ppr modName)) ++ ":"))
          case compileErrors of
            ConvertErrors errs ->
              mapM_
                (S8.putStrLn . S8.pack . ("  " ++) . displayException)
                (nub (NE.toList errs))
            RenameErrors errs ->
              mapM_
                (S8.putStrLn . S8.pack . ("  " ++) . displayException)
                (nub (NE.toList errs)))
       (M.toList errors))

-- | Read the name index.
readIndex :: FilePath -> IO Index
readIndex fp = do
  exists <- doesFileExist fp
  if exists
    then fmap (decode . L.fromStrict) (S.readFile fp)
    else pure
           Index
             { indexGlobals = mempty
             , indexLocals = mempty
             , indexDataCons = mempty
             }

--------------------------------------------------------------------------------
-- Index helpers

lookupGlobalBindingRhs :: Index -> [GlobalBinding] -> Name -> Maybe Rhs
lookupGlobalBindingRhs index bindings name = do
  globalVarId <- M.lookup name (indexGlobals index)
  listToMaybe
    (mapMaybe
       (\case
          GlobalNonRec i rhs | i == globalVarId -> pure rhs
          _ -> Nothing)
       bindings)
