{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Rename the GHC AST to have globally unique names.

module Prana.Rename
  ( renameTopBinding
  , renameId
  , Name
  , RenameFailure(..)
  ) where

import qualified CoreSyn
import           Data.Bifunctor.TH
import           Data.Binary
import           Data.Bitraversable
import           Data.ByteString (ByteString)
import           Data.Int
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Validation
import qualified FastString
import           GHC.Generics
import qualified Module
import qualified Name
import qualified StgSyn
import qualified Unique
import qualified Var

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
  deriving (Eq)

-- | Rename the STG AST to have globally unique names.
renameTopBinding ::
     Module.Module
  -> StgSyn.GenStgTopBinding Var.Id Var.Id
  -> Validation (NonEmpty RenameFailure) (StgSyn.GenStgTopBinding Name Name)
renameTopBinding m =
  bitraverse (validationNel . renameId m) (validationNel . renameId m)

-- | Rename the id to be a globally unique name.
renameId :: Module.Module -> Var.Id -> Either RenameFailure Name
renameId m thing =
  if Name.isInternalName name
    then Left (UnexpectedInternalName name)
    else Right
           (Name
              { namePackage = package
              , nameModule = module'
              , nameName = name'
              , nameUnique =
                  if Var.isExportedId thing
                    then Exported
                    else Unexported
                           (fromIntegral
                              (Unique.getKey (Unique.getUnique name)))
              })
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
