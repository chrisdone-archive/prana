-- | Rename the GHC AST to have globally unique names.

module Prana.Rename where

import           Data.ByteString (ByteString)
import           Data.Int
import qualified StgSyn
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

-- | Rename the STG AST to have globally unique names.
rename :: StgSyn.GenStgTopBinding Var.Id Var.Id -> StgSyn.GenStgTopBinding Name Name
rename = undefined
