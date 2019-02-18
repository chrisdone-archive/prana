{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Print STG in GHC 8.4.3.

module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified CorePrep
import qualified CoreSyn
import qualified CoreToStg
import qualified CostCentre
import qualified DynFlags
import qualified GHC
import qualified GHC.Paths
import qualified HscTypes
import qualified Literal
import qualified SimplStg
import qualified StgSyn as GHC
import qualified TyCon
import qualified Unique

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
           mapM_
             (\modSummary -> do
                stgs <- compile modSummary
                liftIO (print stgs))
             mgraph))

compile ::
     GHC.GhcMonad m
  => GHC.ModSummary
  -> m [GHC.StgTopBinding]
compile modSummary = do
  parsedModule <- GHC.parseModule modSummary
  typecheckedModule <- GHC.typecheckModule parsedModule
  desugared <- GHC.desugarModule typecheckedModule
  let modguts = GHC.dm_core_module desugared
      this_mod = GHC.ms_mod modSummary
  hsc_env <- GHC.getSession
  -- Copied roughly from HcsMain <https://github.com/ghc/ghc/blob/ghc-8.4/compiler/main/HscMain.hs#L1312-L1318>
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

-- Lifted directly from HscMain <https://github.com/ghc/ghc/blob/ghc-8.4/compiler/main/HscMain.hs#L1481-L1493>
myCoreToStg ::
     GHC.DynFlags
  -> GHC.Module
  -> CoreSyn.CoreProgram
  -> IO ([GHC.StgTopBinding], CostCentre.CollectedCCs)
myCoreToStg dflags this_mod prepd_binds = do
  let (stg_binds, cost_centre_info) = CoreToStg.coreToStg dflags this_mod prepd_binds
  stg_binds2 <- SimplStg.stg2stg dflags stg_binds
  return (stg_binds2, cost_centre_info)

--------------------------------------------------------------------------------
-- Orphan Shows for handy quick look

deriving instance Show (GHC.GenStgTopBinding GHC.Id GHC.Id)
deriving instance Show (GHC.GenStgBinding GHC.Id GHC.Id)
instance Show GHC.Id where show i = "(Id " ++ show (Unique.getKey (Unique.getUnique i)) ++ ")"
deriving instance Show (GHC.GenStgRhs GHC.Id GHC.Id)
deriving instance Show (GHC.GenStgExpr GHC.Id GHC.Id)
deriving instance Show (GHC.GenStgArg GHC.Id)
instance Show CostCentre.CostCentreStack where show _ = "CostCentreStack"
instance Show GHC.StgBinderInfo where show _ = "StgBinderInfo"
deriving instance Show GHC.UpdateFlag
instance Show GHC.DataCon where show _ = "DataCon"
deriving instance Show (CoreSyn.Tickish GHC.Id)
instance Show Literal.Literal where show _ = "Literal"
instance Show GHC.Type where show _ = "Type"
instance Show GHC.StgOp where show _ = "StgOp"
deriving instance Show GHC.AltType
deriving instance Show CoreSyn.AltCon
deriving instance Show CostCentre.CostCentre
instance Show GHC.Module where show _ = "Module"
instance Show TyCon.TyCon where show _ = "TyCon"
deriving instance Show CostCentre.IsCafCC
