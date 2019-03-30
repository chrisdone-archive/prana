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
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import           Data.Validation
import qualified Digraph
import qualified DynFlags
import qualified GHC
import           HscTypes
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
  index <- liftIO (readIndex fp)
  (index', errors) <-
    (evalStateT
       (runWriterT
          (do let sccs = Digraph.flattenSCCs mgraph
                  total = length sccs
              mapM_ (compileToPrana total) (zip [1 :: Int ..] sccs)))
       index)
  case errors of
    [] -> liftIO (L.writeFile fp (encode index'))
    _ -> showErrors errors

-- | Compile the module to a prana file and update the index.
compileToPrana ::
     Int
  -> (Int, ModSummary)
  -> WriterT [(GHC.Module, ([GHC.StgTopBinding],CompileError))] (StateT Index GHC.Ghc) ()
compileToPrana total (i, modSummary) = do
  let modName =
        Outputable.showSDocUnsafe (Outputable.ppr mn)
      mn = GHC.ms_mod modSummary
  liftIO
    (putStrLn
       ("[" <> show i <> " of " <> show total <> "] Compiling " <> modName))
  result <- lift (compileModSummary modSummary)
  case result of
    Left compileErrors -> tell [(mn, compileErrors)]
    Right _bindings -> pure ()

-- | Compile the module summary to a set of global bindings, updating
-- the names index too.
compileModSummary ::
     GHC.ModSummary -> StateT Index GHC.Ghc (Either ([GHC.StgTopBinding],CompileError) [GlobalBinding])
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
    Failure errors -> pure (Left (stg_binds, RenameErrors errors))
    Success (bindings, dataCons) -> do
      index <- updateIndex bindings dataCons
      let scope = Scope {scopeIndex = index, scopeModule = module'}
      case runReaderT
             (runConvert (traverse fromGenStgTopBinding bindings))
             scope of
        Failure errs -> pure (Left (stg_binds, ConvertErrors errs))
        Success globals -> pure (Right globals)

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
     (MonadIO m) => [(GHC.Module, ([GHC.StgTopBinding], CompileError))] -> m ()
showErrors errors =
  liftIO
    (mapM_
       (\(modName, (bindings, compileErrors)) -> do
          putStrLn
            ("\nErrors in " ++
             (Outputable.showSDocUnsafe (Outputable.ppr modName)) ++ ":")
          case compileErrors of
            ConvertErrors errs ->
              mapM_
                (putStrLn . ("  " ++) . displayException)
                (nub (NE.toList errs))
            RenameErrors errs ->
              mapM_
                (putStrLn . ("  " ++) . displayException)
                (nub (NE.toList errs))
          when
            False
            (do putStrLn ("Module STG is: ")
                putStrLn (Outputable.showSDocUnsafe (Outputable.ppr bindings))))
       errors)

-- | Read the name index.
readIndex :: FilePath -> IO Index
readIndex fp = do
  exists <- doesFileExist fp
  if exists
    then fmap decode (L.readFile fp)
    else pure
           Index
             { indexGlobals = mempty
             , indexLocals = mempty
             , indexDataCons = mempty
             }
