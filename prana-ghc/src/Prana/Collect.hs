{-# LANGUAGE LambdaCase #-}

-- | Collect information from a module.

module Prana.Collect
  (collectGlobalBindings
  ,collectLocalBindings
  ,collectDataTypes)
  where

import           Control.Monad.Trans.State.Strict
import           Data.Bitraversable
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified DataCon
import qualified Name
import           Prana.Rename
import qualified StgSyn
import qualified TyCon
import qualified Var

-- | Collect the set of globals produced by a set of bindings.
collectGlobalBindings :: [StgSyn.GenStgTopBinding Name Name] -> Set Name
collectGlobalBindings = Set.fromList . concatMap getNames
  where
    getNames =
      \case
        StgSyn.StgTopLifted (StgSyn.StgNonRec bndr _) -> [bndr]
        StgSyn.StgTopLifted (StgSyn.StgRec bndrs) -> map fst bndrs
        StgSyn.StgTopStringLit bndr _ -> [bndr]

-- | Collect the set of locals produced by a set of bindings.
collectLocalBindings :: [StgSyn.GenStgTopBinding Name Name] -> Set Name
collectLocalBindings =
  flip execState mempty .
  traverse (bitraverse (modify . Set.insert) pure) .
  concatMap unwrapToplevel
  where
    unwrapToplevel =
      \case
        StgSyn.StgTopLifted (StgSyn.StgNonRec _ e) -> [e]
        StgSyn.StgTopLifted (StgSyn.StgRec bndrs) -> map snd bndrs
        StgSyn.StgTopStringLit _ _ -> []

-- | Collect the set of data types.
collectDataTypes :: [TyCon.TyCon] -> [(Name.Name, [Var.Id])]
collectDataTypes =
  nub . map (\tyCon -> (Name.getName tyCon, collectDataCons tyCon))

-- | Collect the set of data constructor ids.
collectDataCons :: TyCon.TyCon -> [Var.Id]
collectDataCons =
  nub . map DataCon.dataConWorkId . TyCon.tyConDataCons
