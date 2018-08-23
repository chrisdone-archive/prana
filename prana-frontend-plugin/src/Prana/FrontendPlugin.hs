{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Frontend plugin for GHC.

module Prana.FrontendPlugin
  ( frontendPlugin
  , dumpBindings
  , getBindings
  ) where

import           Bag
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as L
import           Data.Data
import           Data.Generics
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified FastString as GHC
import qualified GHC
import qualified GhcPlugins
import qualified Id as GHC
import qualified Module as GHC
import qualified Name as GHC
import           Prana.Types

frontendPlugin :: GhcPlugins.FrontendPlugin
frontendPlugin = GhcPlugins.defaultFrontendPlugin {
  GhcPlugins.frontend = frontend
  }

frontend :: [String] -> [(String, Maybe GHC.Phase)] -> GHC.Ghc ()
frontend _flags args = do
  targets <- mapM (uncurry GHC.guessTarget) args
  GHC.setTargets targets
  _ <- GHC.load GHC.LoadAllTargets
  dumpBindings id

-- | Track through the module grpah.
dumpBindings :: GHC.GhcMonad m => (Binding -> Binding) -> m ()
dumpBindings f = do
  mgraph <- GHC.getModuleGraph
  mapM_
    (\modSummary -> do
       bs <- track (const (const mempty)) modSummary
       liftIO
         (L.writeFile
            (moduleToFilePath (GHC.ms_mod modSummary))
            (L.toLazyByteString (buildDump (map f bs)))))
    mgraph

-- | Track through the module grpah.
getBindings :: GHC.GhcMonad m => m [Binding]
getBindings = do
  mgraph <- GHC.getModuleGraph
  fmap
    concat
    (mapM (\modSummary -> track (const (const mempty)) modSummary) mgraph)

buildDump :: [Binding] -> L.Builder
buildDump bs = array (map buildBinding bs)

buildBinding :: Binding -> L.Builder
buildBinding b =
  object
    ([ ("id", buildBindingId (bindingId b))
     , ("refs", array (map buildBindingId (bindingRefs b)))
     ] ++
     [("src-span", buildSrcSpan sr) | Just sr <- [bindingSrcSpan b]])

buildSrcSpan :: Span -> L.Builder
buildSrcSpan rs =
  object
    [ ("file", string (spanFile rs))
    , ("start-line", int (spanStartLine rs))
    , ("start-col", int (spanStartCol rs))
    , ("end-line", int (spanEndLine rs))
    , ("end-col", int (spanEndCol rs))
    ]

buildBindingId :: BindingId -> L.Builder
buildBindingId b =
  object
    [ ("package", string (bindingIdPackage b))
    , ("module", string (bindingIdModule b))
    , ("name", string (bindingIdName b))
    ]

array :: [L.Builder] -> L.Builder
array xs = "[" <> mconcat (intersperse "\n," xs) <> "]"

object :: [(ByteString, L.Builder)] -> L.Builder
object keys =
  "{" <>
  mconcat
    (intersperse
       "\n,"
       (map (\(k, v) -> string k <> ": " <> v) keys)) <>
  "}"

int :: Int -> L.Builder
int s = L.byteString (S8.pack (show s))

string :: ByteString -> L.Builder
string s = L.byteString (S8.pack (show s))

-- | Type-check the module and track through it.
track ::
     GHC.GhcMonad m
  => (GHC.Module -> GHC.HsExpr GHC.Id -> Set ByteString)
  -> GHC.ModSummary
  -> m [Binding]
track shouldFlag modSummary = do
  df <- GHC.getSessionDynFlags
  parsedModule <- GHC.parseModule modSummary
  typecheckedModule <- GHC.typecheckModule parsedModule
  let tc = GHC.tm_typechecked_source typecheckedModule
  pure (getBindingsForAll df tc)
  where
    getBindingsForAll df bag = concatMap (getBinding df) (bagToList bag)
    getBinding ::
         GHC.DynFlags -> GHC.Located (GHC.HsBindLR GHC.Id GHC.Id) -> [Binding]
    getBinding df located =
      case GHC.unLoc located of
        GHC.VarBind {GHC.var_id = id', GHC.var_rhs = rhs} ->
          [ Binding
              { bindingFlagged =
                  mconcat
                    (map
                       (shouldFlag module')
                       (listify (not . Set.null . shouldFlag module') rhs))
              , bindingId = idToBindingId (GHC.ms_mod modSummary) id'
              , bindingSrcSpan = locToSpan (GHC.getLoc located)
              , bindingRefs =
                  map
                    (idToBindingId (GHC.ms_mod modSummary) . GHC.unLoc)
                    (referencedIds id' rhs)
              }
          ]
        GHC.FunBind {GHC.fun_id = (GHC.unLoc -> id'), GHC.fun_matches = rhs} ->
          [ Binding
              { bindingFlagged =
                  mconcat
                    (map
                       (shouldFlag module')
                       (listify (not . Set.null . shouldFlag module') rhs))
              , bindingId = idToBindingId module' id'
              , bindingSrcSpan = locToSpan (GHC.getLoc located)
              , bindingRefs =
                  map
                    (idToBindingId module' . GHC.unLoc)
                    (referencedIds id' rhs)
              }
          ]
        GHC.AbsBinds {GHC.abs_binds = binds} -> getBindingsForAll df binds
        GHC.AbsBindsSig {GHC.abs_sig_bind = bind} -> getBinding df bind
        GHC.PatBind {GHC.pat_lhs = lhs, GHC.pat_rhs = rhs} ->
          [ Binding
              { bindingFlagged =
                  mconcat
                    (map
                       (shouldFlag module')
                       (listify (not . Set.null . shouldFlag module') rhs))
              , bindingId = idToBindingId module' id'
              , bindingSrcSpan = locToSpan (GHC.getLoc located)
              , bindingRefs =
                  map
                    (idToBindingId module' . GHC.unLoc)
                    (referencedIds id' rhs)
              }
          | id' <- patIds lhs]
        GHC.PatSynBind {} -> []
    module' = (GHC.ms_mod modSummary)

patIds :: GHC.LPat GHC.Id -> [GHC.Id]
patIds =
  mapMaybe
    (\case
       GHC.VarPat l -> Just (GHC.unLoc l)
       _ -> Nothing) .
  listify (const True)

-- | Get all the referenced variable IDs listed in an AST.
referencedIds :: Data ast => GHC.Id -> ast -> [GHC.Located GHC.Id]
referencedIds ignore =
  nub .
  mapMaybe
    (\case
       GHC.HsVar i -> Just i
       _ -> Nothing) .
  listify
    (\case
       GHC.HsVar i -> GHC.unLoc i /= ignore
       _ -> False)

idToBindingId :: GHC.Module -> GHC.Id -> BindingId
idToBindingId module0 gid =
  BindingId
    { bindingIdPackage = S8.pack packageNameVersion
    , bindingIdModule = S8.pack moduleNameString
    , bindingIdName = S8.pack nameString
    }
  where
    name = GHC.idName gid
    theOccName = GHC.nameOccName name
    module' = GHC.nameModule_maybe name
    nameString = GHC.occNameString theOccName
    unitId = GHC.moduleUnitId (fromMaybe module0 module')
    moduleName = GHC.moduleName (fromMaybe module0 module')
    packageNameVersion = GHC.unitIdString unitId
    moduleNameString = GHC.moduleNameString moduleName

moduleToFilePath :: GHC.Module -> FilePath
moduleToFilePath module' = "bindings_" ++ packageNameVersion ++ "_" ++ moduleNameString ++ ".json"
  where
    unitId = GHC.moduleUnitId module'
    moduleName = GHC.moduleName module'
    packageNameVersion = GHC.unitIdString unitId
    moduleNameString = GHC.moduleNameString moduleName

locToSpan :: GHC.SrcSpan -> Maybe Span
locToSpan =
  \case
    GHC.RealSrcSpan rs ->
      Just
        Span
          { spanFile = GHC.fastStringToByteString (GHC.srcSpanFile rs)
          , spanStartLine = GHC.srcSpanStartLine rs
          , spanStartCol = GHC.srcSpanStartCol rs
          , spanEndLine = GHC.srcSpanEndLine rs
          , spanEndCol = GHC.srcSpanEndCol rs
          }
    GHC.UnhelpfulSpan _ -> Nothing
