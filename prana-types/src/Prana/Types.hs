{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveGeneric #-}

-- |

module Prana.Types where

import           Data.Binary
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
  | ConAppExpr !DataConId ![Arg] ![Type]
  | OpAppExpr !Op ![Arg] !Type
  | CaseExpr !Expr !LocalVarId !Alts
  | LetExpr !LocalBinding !Expr
  | LitExpr !Lit
  deriving (Show, Eq, Generic)

data Alts
  = PolymorphicAlt !Expr
    -- ^ Polymorphic value, we force it.
  | DataAlts !TyCon ![DataAlt] !(Maybe Expr)
    -- ^ For regular ADT types.
  | MultiValAlts !Int ![DataAlt] !(Maybe Expr)
    -- ^ For unboxed sums and unboxed tuples.
  | PrimAlts !PrimRep ![LitAlt] !(Maybe Expr)
    -- ^ Primitive value.
  deriving (Show, Eq, Generic)

data DataAlt =
  DataAlt
    { dataAltCon :: !DataConId
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
  | RhsCon !DataConId ![Arg]
  deriving (Show, Eq, Generic)

newtype GlobalVarId = GlobalVarId Int64
  deriving (Show, Eq, Generic)
instance Binary GlobalVarId

newtype LocalVarId = LocalVarId Int64
  deriving (Show, Eq, Generic)
instance Binary LocalVarId

data SomeVarId
  = SomeLocalVarId !LocalVarId
  | SomeGlobalVarId !GlobalVarId
  | WiredInVal !WiredInVal
  deriving (Show, Eq, Generic)

data WiredInVal
  = WiredIn_coercionToken#
  | WiredIn_void#
  | WiredIn_realWorld#
  | WiredIn_nullAddr#
  | WiredIn_seq
  | WiredIn_magicDict
  | WiredIn_patError
    -- TODO:
    -- Design decision required.
    --
    -- This is something that is actually defined, but it's used by
    -- integer-simple before it's actually defined. I think this may
    -- be a "known-key" rather than "wired-in". I think this can
    -- simply be translated back to its respective GlobalVarId,
    -- although I'm not exactly sure when.
  deriving (Show, Eq, Generic)

data Op =
  Op
  deriving (Show, Eq, Generic)

data DataConId
  = DataConId Int64
  | WiredInCon WiredInCon
  deriving (Show, Eq, Generic)
instance Binary DataConId

data WiredInCon
  = WiredIn_Unit#
  | WiredIn_unboxed_tuple
  deriving (Show, Eq, Generic)
instance Binary WiredInCon

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

--------------------------------------------------------------------------------
-- Binary instances

instance Binary GlobalBinding
instance Binary Rhs
instance Binary UpdateFlag
instance Binary Expr
instance Binary Arg
instance Binary SomeVarId
instance Binary Type
instance Binary Op
instance Binary Alts
instance Binary LocalBinding
instance Binary Lit
instance Binary WiredInVal
instance Binary TyCon
instance Binary DataAlt
instance Binary PrimRep
instance Binary LitAlt
