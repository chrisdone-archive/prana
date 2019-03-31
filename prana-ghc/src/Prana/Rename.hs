{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Rename the GHC AST to have globally unique names.

module Prana.Rename
  ( renameTopBinding
  , renameId
  , displayName
  , Name(..)
  , Unique(..)
  , RenameFailure(..)
  ) where

import           Control.Exception
import qualified CoreSyn
import           Data.Bifunctor.TH
import           Data.Binary
import           Data.Bitraversable
import           Data.ByteString (ByteString)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Int
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Typeable
import           Data.Validation
import qualified DataCon
import           Debug.Trace
import qualified FastString
import           GHC.Generics
import qualified Id
import qualified Module
import qualified Name
import qualified Outputable
import qualified StgSyn
import qualified TyCon
import qualified Unique
import qualified Var

displayName :: Name -> String
displayName (Name pkg md name u) = S8.unpack (pkg <> ":" <> md <> "." <> name <> ext)
  where ext = case u of
                Exported -> ""
                Unexported i -> " (" <> S8.pack (show i) <> ")"

-- | A syntactically globally unique name.
data Name =
  Name
    { namePackage :: {-# UNPACK #-}!ByteString
    , nameModule :: {-# UNPACK #-}!ByteString
    , nameName :: {-# UNPACK #-}!ByteString
    , nameUnique :: !Unique
    }
  deriving (Show, Ord, Eq, Generic)
instance Binary Name

-- | Names can be referred to by their package-module-name
-- combination. However, if it's a local name, then we need an extra
-- unique number to differentiate different instances of the same name
-- string in the same module (e.g. @xs@).
data Unique
  = Exported
  | Unexported !Int64
  deriving (Show, Ord, Eq, Generic)
instance Binary Unique

-- | Some failure in the rename process.
data RenameFailure =
  UnexpectedInternalName !Name.Name
  deriving (Eq, Typeable)

instance Exception RenameFailure where
  displayException =
    \case
      UnexpectedInternalName n ->
        "Unexpected internal name: " ++
        (Outputable.showSDocUnsafe (Outputable.ppr n))

instance Show RenameFailure where
  show (UnexpectedInternalName _) = "UnexpectedInternalName"

-- | Rename the STG AST to have globally unique names.
renameTopBinding ::
     Module.Module
  -> StgSyn.GenStgTopBinding Var.Id Var.Id
  -> Validation (NonEmpty RenameFailure) (StgSyn.GenStgTopBinding Name Name)
renameTopBinding m =
  bitraverse (validationNel . renameId m) (validationNel . renameId m)

-- | Rename the id to be a globally unique name.
--
-- For some reason, newtype constructors appear in the AST even at
-- this stage. So we replace them with id. I asked ghc-devs, but again
-- got no answer:
-- <https://mail.haskell.org/pipermail/ghc-devs/2019-March/017384.html>
-- So hopefully this just works.
renameId :: Module.Module -> Var.Id -> Either RenameFailure Name
renameId m thing =
  if Name.isInternalName name
    then Left (UnexpectedInternalName name)
    else
           (let x =
                  Name
                    { namePackage = package
                    , nameModule = module'
                    , nameName = name'
                    , nameUnique =
                        if Var.isExportedId thing
                          then Exported
                          else Unexported
                                 (fromIntegral
                                    (Unique.getKey (Unique.getUnique name)))
                    }
             in (if x==Name "base" "GHC.Exception" "errorCallWithCallStackException" Exported
                    then id -- trace (displayName x ++ " is defined here!")
                    else id) (if isNewtypeConstructor thing
                                         then let y =
                                                    Name
                                                      { namePackage = "base"
                                                      , nameModule = "GHC.Base"
                                                      , nameName = "id"
                                                      , nameUnique = Exported
                                                      }
                                               in -- trace
                                                  --   ("Replacing " ++ displayName x ++ " with " ++ displayName y)
                                                    (Right y)
                                         else Right x))
  where
    package =
      FastString.fs_bs
        (Module.unitIdFS (Module.moduleUnitId (Name.nameModule name)))
    module' =
      FastString.fs_bs
        (Module.moduleNameFS (Module.moduleName (Name.nameModule name)))
    name' = FastString.fs_bs (Name.getOccFS name)
    name =
      case Name.nameModule_maybe n of
        Nothing -> qualifyName n
        Just {} -> n
      where
        n = Name.getName thing
    qualifyName :: Name.Name -> Name.Name
    qualifyName n =
      Name.mkExternalName
        (Unique.getUnique n)
        m
        (Name.nameOccName n)
        (Name.nameSrcSpan n)

--------------------------------------------------------------------------------
-- Predicates

-- | Is this a newtype constructor?
isNewtypeConstructor :: Id.Id -> Bool
isNewtypeConstructor i =
  case Id.isDataConId_maybe i of
    Nothing -> False
    Just dc -> TyCon.isNewTyCon (DataCon.dataConTyCon dc)

--------------------------------------------------------------------------------
-- Orphans: Easily removable if these instances are ever provided
-- upstream.

deriveBifunctor ''StgSyn.GenStgTopBinding
deriveBifunctor ''StgSyn.GenStgBinding
deriveBifunctor ''StgSyn.GenStgRhs
deriveBifunctor ''StgSyn.GenStgExpr
deriving instance Functor StgSyn.GenStgArg
deriving instance Functor CoreSyn.Tickish

deriveBitraversable ''StgSyn.GenStgTopBinding
deriveBitraversable ''StgSyn.GenStgBinding
deriveBitraversable ''StgSyn.GenStgRhs
deriveBitraversable ''StgSyn.GenStgExpr
deriving instance Traversable StgSyn.GenStgArg
deriving instance Traversable CoreSyn.Tickish

deriveBifoldable ''StgSyn.GenStgTopBinding
deriveBifoldable ''StgSyn.GenStgBinding
deriveBifoldable ''StgSyn.GenStgRhs
deriveBifoldable ''StgSyn.GenStgExpr
deriving instance Foldable StgSyn.GenStgArg
deriving instance Foldable CoreSyn.Tickish
