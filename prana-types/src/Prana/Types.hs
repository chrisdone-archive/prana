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
  | OpAppExpr !Op ![Arg] !PrimOpType
  | CaseExpr !Expr !LocalVarId !Alts
  | LetExpr !LocalBinding !Expr
  | LitExpr !Lit
  deriving (Show, Eq, Generic)

-- The Maybe Expr is the DEFAULT case.
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
  = RhsClosure !Closure
  | RhsCon !Con
  deriving (Show, Eq, Generic)

data Con =
  Con
    { conDataCon :: !DataConId
    , conArg :: ![Arg]
    }
  deriving (Show, Eq, Generic)

data Closure =
  Closure
    { closureFreeVars :: ![LocalVarId]
    , closureUpdateFlag :: !UpdateFlag
    , closureParams :: ![LocalVarId]
    , closureExpr :: !Expr
    } deriving (Show, Eq, Generic)

newtype GlobalVarId = GlobalVarId Int64
  deriving (Show, Eq, Generic, Ord)
instance Binary GlobalVarId

newtype LocalVarId = LocalVarId Int64
  deriving (Show, Eq, Generic, Ord)
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
  | WiredIn_proxy#
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


data DataConId
  = DataConId !Int64
  | UnboxedTupleConId !Int
  deriving (Show, Eq, Generic, Ord)
instance Binary DataConId

data UpdateFlag
  = ReEntrant
  | Updatable
  | SingleEntry
  deriving (Show, Eq, Generic)

data Type =
  Type
  deriving (Show, Eq, Generic)

data PrimOpType
  = BoolType
  | UnknownType
  deriving (Show, Eq, Generic)

data Arg
  = VarArg !SomeVarId
  | LitArg !Lit
  deriving (Show, Eq, Generic)

data Lit
  = IntLit !Int -- Machine int.
  | UnknownLit
  deriving (Show, Eq, Generic)

data PrimRep
  = VoidRep
  | LiftedRep
  | UnliftedRep -- ^ Unlifted pointer
  | IntRep -- ^ Signed, word-sized value
  | WordRep -- ^ Unsigned, word-sized value
  | Int64Rep -- ^ Signed, 64 bit value (with 32-bit words only)
  | Word64Rep -- ^ Unsigned, 64 bit value (with 32-bit words only)
  | AddrRep -- ^ A pointer, but /not/ to a Haskell value (use '(Un)liftedRep')
  | FloatRep
  | DoubleRep
  | VecRep Int PrimElemRep -- ^ A vector
  deriving (Eq, Show, Generic)

data PrimElemRep
  = Int8ElemRep
  | Int16ElemRep
  | Int32ElemRep
  | Int64ElemRep
  | Word8ElemRep
  | Word16ElemRep
  | Word32ElemRep
  | Word64ElemRep
  | FloatElemRep
  | DoubleElemRep
   deriving( Eq, Show, Generic )

data TyCon =
  TyCon
  deriving (Show, Eq, Generic)

data Op
  = PrimOp PrimOp
  | OtherOp
  deriving (Show, Eq, Generic)

data PrimOp
  = UnknownPrimOp String
  | IntNegOp
  | IntAddOp
  | IntSubOp
  | IntEqOp
  | IntLtOp
  | IntSubCOp
  | TagToEnumOp
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Binary instances

instance Binary GlobalBinding
instance Binary Rhs
instance Binary UpdateFlag
instance Binary Expr
instance Binary PrimOpType
instance Binary Arg
instance Binary SomeVarId
instance Binary Type
instance Binary Op
instance Binary PrimOp
instance Binary Alts
instance Binary LocalBinding
instance Binary Lit
instance Binary WiredInVal
instance Binary TyCon
instance Binary DataAlt
instance Binary PrimRep
instance Binary PrimElemRep
instance Binary LitAlt
instance Binary Closure
instance Binary Con
