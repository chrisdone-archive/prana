{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Reading the primops.

module Prana.PrimOp
  ( derivePrimOpType
  , parsePrimops
  , findProjectFile
  , Entry(..)
  , Ty(..)
  , TyCon(..)
  , TyVar
  ) where

import Data.Maybe
import GHC.Generics
import Language.Haskell.TH
import Parser
import Syntax
import System.Directory
import System.FilePath

--------------------------------------------------------------------------------
-- Derive the PrimOp type

derivePrimOpType :: Q [Dec]
derivePrimOpType = do
  entries <- runIO parsePrimops
  conses <- fmap catMaybes (mapM mkCons entries)
  pure [DataD ctx (mkName "PrimOp") tyVars mkind conses derivings]
  where
    ctx = []
    tyVars = []
    mkind = Nothing
    mkCons entry =
      case entry of
        PrimOpSpec {cons = constructorName} ->
          pure (Just (NormalC (mkName constructorName) slots))
        _ -> pure Nothing
    slots = []
    derivings =
      [ DerivClause
          Nothing
          [ ConT ''Show
          , ConT ''Generic
          , ConT ''Eq
          , ConT ''Ord
          , ConT ''Read
          , ConT ''Enum
          , ConT ''Bounded
          ]
      ]

--------------------------------------------------------------------------------
-- Parsing the primops file

parsePrimops :: IO [Entry]
parsePrimops = do
  fp <- findProjectFile "prana-primops/primops.txt"
  bytes <- readFile fp
  case parse bytes of
    Left err -> error err
    Right (Info _ entries) -> pure entries

--------------------------------------------------------------------------------
-- TH helpers

findProjectFile :: FilePath -> IO FilePath
findProjectFile relativeFile =
  do mstack <- findStack
     case mstack of
       Nothing -> error "Could not find stack.yaml."
       Just stackDir -> pure (stackDir <> "/" <> relativeFile)

findStack :: IO (Maybe FilePath)
findStack = do
  pwd <- getCurrentDirectory
  go pwd
  where
    go pwd = do
      exists <- doesFileExist (pwd <> "/stack.yaml")
      if exists
        then pure (Just pwd)
        else let parent = takeDirectory pwd
             in if parent /= pwd
                   then go parent
                   else pure Nothing
