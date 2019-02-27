{-# LANGUAGE LambdaCase #-}

-- | Collect information from a module.

module Prana.Collect
  (collectGlobalBindings
  ,collectLocalBindings)
  where

import           Control.Monad.State
import           Data.Bitraversable
import           Data.Set (Set)
import qualified Data.Set as Set
import           Prana.Rename
import qualified StgSyn

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
