{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Import from GHC into Prana.

module Prana.Ghc
  {-( fromGenStgTopBinding
  , runConvert
  , Convert
  , ConvertError(..)
  )-} where

import           Control.Monad.Trans.Reader
import qualified CoreSyn
import           Data.ByteString (ByteString)
import           Data.Int
import           Data.Maybe
import           Data.Validation
import qualified DataCon
import qualified FastString
import qualified GHC
import qualified Module
import qualified Name
import           Prana.Types
import qualified StgSyn
import qualified Unique
import qualified Var

-- Pipeline:
--
-- 1. RENAME each module. ✓
--
-- 2. COLLECT each module (get data cons, get globals, get locals).
--
-- 3. RECONSTRUCT each module.

-- -- --------------------------------------------------------------------------------
-- -- -- Convert monad

-- data ConvertError
--   = UnexpectedPolymorphicCaseAlts
--   | UnexpectedLambda
--   | UnexpectedInternalName
--   | UnknownVariable
--   deriving (Show, Eq)

-- data Db =
--   Db
--     { dbExportedVarIds :: [ExportedName]
--     , dbUnexportedIds :: [UnexportedName]
--     }

-- --------------------------------------------------------------------------------
-- -- Convert GHC ids to exported or unexported IDS

-- toConstrName :: Module.Module -> DataCon.DataCon -> Validation [ConvertError] ConstrName
-- toConstrName m =
--   fmap ConstrName . toExportedName m . DataCon.dataConWorkId

-- toExportedName :: Module.Module -> Var.Id -> Validation [ConvertError] ExportedName
-- toExportedName m thing =
--   if Name.isInternalName name
--     then Failure [UnexpectedInternalName]
--     else pure (ExportedName package module' name')
--   where
--     package =
--       FastString.fs_bs
--         (Module.unitIdFS (Module.moduleUnitId (Name.nameModule name)))
--     module' =
--       FastString.fs_bs
--         (Module.moduleNameFS (Module.moduleName (Name.nameModule name)))
--     name' = FastString.fs_bs (Name.getOccFS name)
--     name =
--       case Name.nameModule_maybe n of
--         Nothing -> qualifyName m n
--         Just {} -> n
--       where
--         n = Name.getName thing

-- toUnexportedName :: Module.Module -> Var.Id -> Validation [ConvertError] UnexportedName
-- toUnexportedName m thing =
--   if Name.isInternalName name
--     then Failure [UnexpectedInternalName]
--     else pure
--            (UnexportedName
--               package
--               module'
--               name'
--               (Unique (fromIntegral (Unique.getKey (Unique.getUnique name)))))
--   where
--     package =
--       FastString.fs_bs
--         (Module.unitIdFS (Module.moduleUnitId (Name.nameModule name)))
--     module' =
--       FastString.fs_bs
--         (Module.moduleNameFS (Module.moduleName (Name.nameModule name)))
--     name' = FastString.fs_bs (Name.getOccFS name)
--     name =
--       case Name.nameModule_maybe n of
--         Nothing -> qualifyName m n
--         Just {} -> n
--       where
--         n = Name.getName thing

-- qualifyName :: Module.Module -> Name.Name -> Name.Name
-- qualifyName m name =
--   Name.mkExternalName
--     (Unique.getUnique name)
--     m
--     (Name.nameOccName name)
--     (Name.nameSrcSpan name)
