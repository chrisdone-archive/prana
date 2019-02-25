-- | Rename the GHC AST to have globally unique names.

module Prana.Rename
  ( renameTopBinding
  , renameId
  , Name
  ) where

import           Data.ByteString (ByteString)
import           Data.Int
import qualified FastString
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
  deriving (Show, Ord, Eq)

-- | Names can be referred to by their package-module-name
-- combination. However, if it's a local name, then we need an extra
-- unique number to differentiate different instances of the same name
-- string in the same module (e.g. @xs@).
data Unique
  = Exported
  | Unexported !Int64
  deriving (Show, Ord, Eq)

-- | Some failure in the rename process.
data RenameFailure =
  UnexpectedInternalName !Name.Name
  deriving (Eq)

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

-- | Rename the STG AST to have globally unique names.
renameTopBinding ::
     Module.Module
  -> StgSyn.GenStgTopBinding Var.Id Var.Id
  -> StgSyn.GenStgTopBinding Name Name
renameTopBinding = undefined
