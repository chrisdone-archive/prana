{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |

module Prana.Types where

import           Data.Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Data (Data, Typeable)
import           Data.Int
import           GHC.Generics

data GlobalBinding
  = GlobalStringLit !GlobalVarId !ByteString
  | GlobalNonRec !GlobalVarId !Rhs
  | GlobalRec ![(GlobalVarId, Rhs)]
  deriving (Show, Eq, Generic)

data LocalBinding
  = LocalNonRec !LocalVarId !Rhs
  | LocalRec ![(LocalVarId, Rhs)]
  deriving (Show, Eq, Generic)

data Expr
  = AppExpr !SomeVarId ![Arg]
  | ConAppExpr !DataCon ![Arg] ![Type]
  | CaseExpr !Expr !LocalVarId !Alts
  | LetExpr !LocalBinding !Expr
  | Unknown
  deriving (Show, Eq, Generic)

data Alts
  = PolymorphicAlt !Expr
  | DataAlts !TyCon ![DataAlt] !(Maybe Expr)
  | MultiValAlts !Int !TyCon ![DataAlt] !(Maybe Expr)
  | PrimAlts !PrimRep ![LitAlt] !(Maybe Expr)
  deriving (Show, Eq, Generic)

data DataAlt =
  DataAlt
    { dataAltCon :: !DataCon
    , dataAltBinders :: ![LocalVarId]
    , dataAltExpr :: !Expr
    }
  deriving (Show, Eq, Generic)

data LitAlt =
  LitAlt
    { litAltLit :: !Lit
    , litAltBinders :: ![LocalVarId]
    , litAltExpr :: !Expr
    }
  deriving (Show, Eq, Generic)

data Rhs
  = RhsClosure
      ![LocalVarId] -- Free variables.
      !UpdateFlag
      ![LocalVarId] -- Parameters.
      !Expr
  | RhsCon !DataCon ![Arg]
  deriving (Show, Eq, Generic)

newtype GlobalVarId = GlobalVarId Int64
  deriving (Show, Eq, Generic)

newtype LocalVarId = LocalVarId Int64
  deriving (Show, Eq, Generic)

data SomeVarId
  = SomeLocalVarId !LocalVarId
  | SomeGlobalVarId !GlobalVarId
  deriving (Show, Eq, Generic)

data Op =
  Op
  deriving (Show, Eq, Generic)

data DataCon =
  DataCon
  deriving (Show, Eq, Generic)

data UpdateFlag
  = ReEntrant
  | Updatable
  | SingleEntry
  deriving (Show, Eq, Generic)

data Type =
  Type
  deriving (Show, Eq, Generic)

data Arg
  = VarArg !SomeVarId
  | LitArg !Lit
  deriving (Show, Eq, Generic)

data Lit =
  Lit
  deriving (Show, Eq, Generic)

data PrimRep =
  PrimRep
  deriving (Show, Eq, Generic)

data TyCon =
  TyCon
  deriving (Show, Eq, Generic)
