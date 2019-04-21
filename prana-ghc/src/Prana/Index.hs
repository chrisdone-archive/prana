{-# LANGUAGE DeriveGeneric #-}

-- | Name indexes.

module Prana.Index
  ( updateIndex
  , Index(..)
  , reverseIndex
  , ReverseIndex(..)
  ) where

import           Control.Monad.State
import           Data.Binary (Binary)
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Tuple
import           GHC.Generics
import           Prana.Collect
import           Prana.Rename
import           Prana.Types
import qualified StgSyn

data Index =
  Index
    { indexGlobals :: Map Name GlobalVarId
    , indexLocals :: Map Name LocalVarId
    , indexDataCons :: Map Name DataConId
    }
  deriving (Generic, Show)
instance Binary Index

data ReverseIndex =
  ReverseIndex
    { reverseIndexDataCons :: Map DataConId Name
    , reverseIndexGlobals :: Map GlobalVarId Name
    , reverseIndexLocals :: Map LocalVarId Name
    }

updateIndex ::
     Monad m
  => [StgSyn.GenStgTopBinding Name Name]
  -> [Name]
  -> StateT Index m Index
updateIndex bindings tycons =
  do modify
       (\idx ->
          idx
            { indexGlobals =
                let start = fromIntegral (M.size (indexGlobals idx))
                 in foldl'
                      (\m (k, i) -> M.insert k (GlobalVarId i) m)
                      (indexGlobals idx)
                      (zip (Set.toList (collectGlobalBindings bindings)) [start ..])
            , indexLocals =
                let start = fromIntegral (M.size (indexLocals idx))
                 in foldl'
                      (\m (k, i) -> M.insert k (LocalVarId i) m)
                      (indexLocals idx)
                      (zip (Set.toList (collectLocalBindings bindings)) [start ..])
            , indexDataCons =
                let start = fromIntegral (M.size (indexDataCons idx))
                 in foldl'
                      (\m (k, i) -> M.insert k (DataConId i) m)
                      (indexDataCons idx)
                      (zip tycons [start ..])
            })
     get

reverseIndex :: Index -> ReverseIndex
reverseIndex index =
  ReverseIndex
    { reverseIndexDataCons =
        M.fromList (map swap (M.toList (indexDataCons index)))
    , reverseIndexLocals =
        M.fromList (map swap (M.toList (indexLocals index)))
    , reverseIndexGlobals =
        M.fromList (map swap (M.toList (indexGlobals index)))
    }
