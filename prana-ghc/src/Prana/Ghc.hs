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

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified CorePrep
import qualified CoreSyn
import qualified CoreToStg
import qualified CostCentre
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import           Data.Validation
import qualified Digraph
import qualified DynFlags
import qualified GHC
import qualified HscTypes
import qualified Outputable
import           Prana.Collect
import           Prana.Index
import           Prana.Reconstruct
import           Prana.Rename
import           Prana.Types
import qualified SimplStg
import qualified StgSyn as GHC
import qualified TyCon

data CompileError
  = RenameErrors (NonEmpty (RenameFailure))
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
  mgraph <-
    fmap (\g -> GHC.topSortModuleGraph False g Nothing) GHC.getModuleGraph
  _index <-
    execStateT
      (mapM_
         (\modSummary -> do
            liftIO
              (putStrLn
                 ("Compiling " ++
                  Outputable.showSDocUnsafe
                    (Outputable.ppr (GHC.ms_mod modSummary))))
            result <- compileModSummary modSummary
            case result of
              Left compileErrors ->
                liftIO
                  (case compileErrors of
                     ConvertErrors errs -> mapM_ print (nub (NE.toList errs))
                     RenameErrors errs -> mapM_ print (nub (NE.toList errs)))
              Right _bindings -> pure ())
         (Digraph.flattenSCCs mgraph))
      (Index
         {indexGlobals = mempty, indexLocals = mempty, indexDataCons = mempty})
  pure ()

-- | Compile the module summary to a set of global bindings, updating
-- the names index too.
compileModSummary ::
     GHC.ModSummary -> StateT Index GHC.Ghc (Either CompileError [GlobalBinding])
compileModSummary modSummary = do
  parsedModule <- lift (GHC.parseModule modSummary)
  typecheckedModule <- lift (GHC.typecheckModule parsedModule)
  desugared <- lift (GHC.desugarModule typecheckedModule)
  topBindings <- lift (desugaredToStg modSummary desugared)
  let module' = GHC.ms_mod modSummary
      modguts = GHC.dm_core_module desugared
      tyCons = collectDataCons (HscTypes.mg_tcs modguts)
  case (,) <$> traverse (renameTopBinding module') topBindings <*>
       traverse (validationNel . renameId module') (Set.toList tyCons) of
    Failure errors -> pure (Left (RenameErrors errors))
    Success (bindings, tycons) -> do
      index <- updateIndex bindings tycons
      let scope = Scope {scopeIndex = index, scopeModule = module'}
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
