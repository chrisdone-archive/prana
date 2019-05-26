{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Name indexes.

module Prana.Index
  ( updateIndex
  , Index(..)
  , reverseIndex
  , ReverseIndex(..)
  ) where

import           Control.Monad.Trans.State.Strict
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Tuple
import           Prana.Collect
import           Prana.Rename
import           Prana.Types
import qualified StgSyn

updateIndex ::
     Monad m
  => [StgSyn.GenStgTopBinding Name Name]
  -> [(Name, [Name])]
  -> StateT Index m Index
updateIndex bindings tycons = do
  modify
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
                   (\m (typeIdx, (_typeName, constructorNames)) ->
                      foldl'
                        (\acc (consIndex, consName) ->
                           M.insert
                             consName
                             (DataConId (TypeId typeIdx) (ConIndex consIndex))
                             acc)
                        m
                        (zip [0 ..] constructorNames))
                   (indexDataCons idx)
                   (zip [start ..] tycons)
         , indexTypes =
             let start = fromIntegral (M.size (indexTypes idx))
              in foldl'
                   (\m ((typeName, _constructorNames), i) ->
                      M.insert typeName (TypeId i) m)
                   (indexTypes idx)
                   (zip tycons [start ..])
         })
  get

reverseIndex :: Index -> ReverseIndex
reverseIndex index =
  ReverseIndex
    { reverseIndexTypes = M.fromList (map swap (M.toList (indexTypes index)))
    , reverseIndexDataCons =
        M.fromList (map swap (M.toList (indexDataCons index)))
    , reverseIndexLocals = M.fromList (map swap (M.toList (indexLocals index)))
    , reverseIndexGlobals =
        M.fromList (map swap (M.toList (indexGlobals index)))
    , reverseIndexIndex = index
    }
