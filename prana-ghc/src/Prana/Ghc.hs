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
  ( compileModSummary
  , compileModuleGraph
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
import           Data.Binary (encode, decode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Validation
import qualified Digraph
import qualified DynFlags
import qualified GHC
import           HscTypes
import qualified HscTypes
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

data CompileError
  = RenameErrors (NonEmpty RenameFailure)
  | ConvertErrors (NonEmpty ConvertError)
  deriving (Show)

-- | Compile all modules in the graph.
--
-- 1) Load up the names index.
-- 2) Compile each module in the graph.
-- 3) Write back out the names index.
-- 4) Write out the files.
compileModuleGraph :: GHC.Ghc ()
compileModuleGraph = do
  dflags <- GHC.getSessionDynFlags
  GHC.setSessionDynFlags dflags {DynFlags.hscTarget = DynFlags.HscAsm}
  mgraph <-
    fmap (\g -> GHC.topSortModuleGraph False g Nothing) GHC.getModuleGraph
  fp <- liftIO (getEnv "PRANA_INDEX")
  index <-
    liftIO
      (do exists <- doesFileExist fp
          if exists
            then fmap decode (L.readFile fp)
            else pure
                   Index
                     { indexGlobals = mempty
                     , indexLocals = mempty
                     , indexDataCons = mempty
                     })
  (index', errors) <-
    (evalStateT
       (runWriterT
          (do let sccs = Digraph.flattenSCCs mgraph
                  total = length sccs
              mapM_
                (\(i, modSummary) -> do
                   let modName =
                         Outputable.showSDocUnsafe
                           (Outputable.ppr (GHC.ms_mod modSummary))
                   liftIO (putStrLn ("[" <> show i <> " of " <> show total <> "] Compiling " <> modName))
                   result <- lift (compileModSummary modSummary)
                   case result of
                     Left compileErrors -> tell [(modName, compileErrors)]
                     Right _bindings -> pure ())
                (zip [1 :: Int ..] sccs)))
       index)
  -- liftIO
  --   (S8.putStrLn
  --      (mconcat
  --         (intersperse
  --            ", "
  --            (nubbed (map nameName (M.keys (indexGlobals index)))))))
  -- liftIO
  --   (S8.putStrLn
  --      (mconcat
  --         (intersperse ", " (nubbed (map nameName (M.keys (indexLocals index)))))))
  case errors of
    [] -> liftIO (L.writeFile fp (encode index'))
    _ ->
      liftIO
        (mapM_
           (\(modName, compileErrors) -> do
              putStrLn ("\nErrors in " ++ modName ++ ":")
              case compileErrors of
                ConvertErrors errs ->
                  mapM_
                    (putStrLn . ("  " ++) . displayException)
                    (nub (NE.toList errs))
                RenameErrors errs ->
                  mapM_
                    (putStrLn . ("  " ++) . displayException)
                    (nub (NE.toList errs)))
           errors)
  pure ()
  -- where
  --   nubbed = Set.toList . Set.fromList

-- | Compile the module summary to a set of global bindings, updating
-- the names index too.
compileModSummary ::
     GHC.ModSummary -> StateT Index GHC.Ghc (Either CompileError [GlobalBinding])
compileModSummary modSum = do
  pmod <- lift $ GHC.parseModule modSum      -- ModuleSummary
  tmod <- lift $  GHC.typecheckModule pmod    -- TypecheckedSource
  dmod <- lift $  GHC.desugarModule tmod      -- DesugaredModule
  let core = GHC.coreModule dmod      -- CoreModule
  let cb = HscTypes.mg_binds core -- [CoreBind]
  -- liftIO (putStrLn $ showPpr unsafeGlobalDynFlags cb)
  hsc_env <- lift $  GHC.getSession
  let modguts = GHC.dm_core_module dmod
      this_mod = GHC.ms_mod modSum
  (CgGuts {cg_binds = core_binds, cg_tycons = tycons}, modDetails) <-
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
  -- liftIO (putStrLn $ showPpr unsafeGlobalDynFlags prepd_binds)
  -- YES!!!!!!!!!!!!!!!!!
  dflags <- lift $  DynFlags.getDynFlags
  (stg_binds, _) <- liftIO (myCoreToStg dflags this_mod prepd_binds)
  -- liftIO (liftIO (putStrLn $ Outputable.showPpr DynFlags.unsafeGlobalDynFlags stg_binds))
  pure (Right [])



  -- parsedModule <- lift (GHC.parseModule modSummary)
  -- typecheckedModule <- lift (GHC.typecheckModule parsedModule)
  -- desugared <- lift (GHC.desugarModule typecheckedModule)
  -- topBindings <- lift (desugaredToStg modSummary desugared)
  -- liftIO (liftIO (putStrLn $ Outputable.showPpr DynFlags.unsafeGlobalDynFlags topBindings))
  let module' = GHC.ms_mod modSum
      modguts = GHC.dm_core_module dmod
      tyCons = collectDataCons (HscTypes.mg_tcs modguts)
  {-liftIO (putStrLn (Outputable.showSDocUnsafe (Outputable.ppr (GHC.ms_mod modSum)) ++
                    ": Scanning"))-}
  case (,) <$> traverse (renameTopBinding module') stg_binds <*>
       traverse (validationNel . renameId module') (Set.toList tyCons) of
    Failure errors -> pure (Left (RenameErrors errors))
    Success (bindings, tycons) -> do
      -- liftIO (putStrLn ("Updating index w/ " ++ intercalate ", " (map (S8.unpack . nameName) tycons)))
      {-liftIO (putStrLn (Outputable.showSDocUnsafe (Outputable.ppr (GHC.ms_mod modSum)) ++
                        ": Indexing"))-}
      index <- updateIndex bindings tycons
      let scope = Scope {scopeIndex = index, scopeModule = module'}
      {-liftIO (putStrLn (Outputable.showSDocUnsafe (Outputable.ppr (GHC.ms_mod modSum)) ++
                  ": Rewriting"))-}
      case runReaderT
             (runConvert (traverse fromGenStgTopBinding bindings))
             scope of
        Failure errs -> pure (Left (ConvertErrors errs))
        Success globals -> pure (Right globals)

-- | Convert a desguared Core AST to STG.
desugaredToStg ::
     GHC.GhcMonad m
  => HscTypes.ModSummary
  -> GHC.DesugaredModule
  -> m [GHC.StgTopBinding]
desugaredToStg modSummary desugared = do
  hsc_env <- GHC.getSession
  (prepd_binds, _) <-
    liftIO
      (CorePrep.corePrepPgm
         hsc_env
         this_mod
         (GHC.ms_location modSummary)
         (HscTypes.mg_binds modguts)
         (filter TyCon.isDataTyCon (HscTypes.mg_tcs modguts)))
  dflags <- DynFlags.getDynFlags
  (stg_binds, _) <- liftIO (myCoreToStg dflags this_mod prepd_binds)
  pure stg_binds
  where modguts = GHC.dm_core_module desugared
        this_mod = GHC.ms_mod modSummary

-- -- | Perform core to STG transformation.
-- myCoreToStg ::
--      GHC.DynFlags
--   -> GHC.Module
--   -> CoreSyn.CoreProgram
--   -> IO ([GHC.StgTopBinding], CostCentre.CollectedCCs)
-- myCoreToStg dflags this_mod prepd_binds = do
--   let (stg_binds, cost_centre_info) = CoreToStg.coreToStg dflags this_mod prepd_binds
--   stg_binds2 <- SimplStg.stg2stg dflags stg_binds
--   return (stg_binds2, cost_centre_info)

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
