{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveGeneric #-}

-- |

module Prana.Types
  ( module Prana.Types
  , module Prana.PrimOp.Type
  ) where

import           Data.Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Int
import           Data.Map.Strict (Map)
import           GHC.Generics
import           Prana.PrimOp.Type

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
  | OpAppExpr !Op ![Arg]
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

data SomeTypeId

data TypeId
  = TypeId
      { typeIdInt :: !Int64
      }
  | WiredInType !WiredInType
  deriving (Show, Eq, Generic, Ord)
instance Binary TypeId

data WiredInType
  = WiredIn_CharPrimTyConName
  | WiredIn_IntPrimTyConName
  | WiredIn_Int32PrimTyConName
  | WiredIn_Int64PrimTyConName
  | WiredIn_WordPrimTyConName
  | WiredIn_Word32PrimTyConName
  | WiredIn_Word64PrimTyConName
  | WiredIn_AddrPrimTyConName
  | WiredIn_FloatPrimTyConName
  | WiredIn_DoublePrimTyConName
  | WiredIn_StatePrimTyConName
  | WiredIn_ProxyPrimTyConName
  | WiredIn_RealWorldTyConName
  | WiredIn_ArrayPrimTyConName
  | WiredIn_ArrayArrayPrimTyConName
  | WiredIn_SmallArrayPrimTyConName
  | WiredIn_ByteArrayPrimTyConName
  | WiredIn_MutableArrayPrimTyConName
  | WiredIn_MutableByteArrayPrimTyConName
  | WiredIn_MutableArrayArrayPrimTyConName
  | WiredIn_SmallMutableArrayPrimTyConName
  | WiredIn_MutVarPrimTyConName
  | WiredIn_MVarPrimTyConName
  | WiredIn_TVarPrimTyConName
  | WiredIn_StablePtrPrimTyConName
  | WiredIn_StableNamePrimTyConName
  | WiredIn_CompactPrimTyConName
  | WiredIn_BcoPrimTyConName
  | WiredIn_WeakPrimTyConName
  | WiredIn_ThreadIdPrimTyConName
  | WiredIn_EqPrimTyConName
  | WiredIn_EqReprPrimTyConName
  | WiredIn_EqPhantPrimTyConName
  | WiredIn_VoidPrimTyConName
  | WiredIn_UnboxedTuple !Int
  deriving (Show, Eq, Generic, Ord)
instance Binary WiredInType

newtype ConIndex =
  ConIndex
    { conIndexInt :: Int64
    }
  deriving (Show, Eq, Generic, Ord)
instance Binary ConIndex

data DataConId
  = DataConId !TypeId !ConIndex
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

data Arg
  = VarArg !SomeVarId
  | LitArg !Lit
  deriving (Show, Eq, Generic)

data Lit
  = CharLit !Char
  | StringLit !ByteString
  | NullAddrLit
  | IntLit !Int
  | Int64Lit !Int64
  | WordLit !Word
  | Word64Lit !Word64
  | FloatLit !Float
  | DoubleLit !Double
  | IntegerLit !Integer
  | LabelLit
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
  deriving (Eq, Show, Generic)

data TyCon =
  TyCon
  deriving (Show, Eq, Generic)

data Op
  = PrimOp !PrimOp !(Maybe TypeId)
  | ForeignOp !CCallSpec !FFIReturnType
  | OtherOp
  deriving (Show, Eq, Generic)

data FFIType
  = FFI_Char
  | FFI_Int
  | FFI_Int32
  | FFI_Int64
  | FFI_Word
  | FFI_Word32
  | FFI_Word64
  | FFI_Addr
  | FFI_Float
  | FFI_Double
  | FFI_StablePtr
  deriving (Show, Eq, Generic)

data FFIReturnType =
  FFIUnboxedTupleOfStateRealWorldAnd !(Maybe FFIType)
  deriving (Show, Eq, Generic)

displayName :: Name -> String
displayName (Name pkg md name u) = S8.unpack (pkg <> ":" <> md <> "." <> name <> ext)
  where ext = case u of
                Exported -> ""
                Unexported i -> "_" <> S8.pack (show i) <> ""

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

data Index =
  Index
    { indexGlobals :: Map Name GlobalVarId
    , indexLocals :: Map Name LocalVarId
    , indexDataCons :: Map Name DataConId
    , indexTypes :: Map Name TypeId
    }
  deriving (Generic, Show)
instance Binary Index

data ReverseIndex =
  ReverseIndex
    { reverseIndexDataCons :: Map DataConId Name
    , reverseIndexGlobals :: Map GlobalVarId Name
    , reverseIndexLocals :: Map LocalVarId Name
    , reverseIndexTypes :: Map TypeId Name
    , reverseIndexIndex :: Index
    }

--------------------------------------------------------------------------------
-- Foreign types

data CCallSpec =
  CCallSpec
    { cCallTarget :: !CCallTarget
    , cCallConv :: !CCallConv
    , safety :: !Safety
    , unique :: !Unique
    }
  deriving (Eq, Generic, Show)

data SourceText
  = SourceText String | NoSourceText
  deriving( Eq , Generic, Show)

data CCallTarget
  -- An "unboxed" ccall# to named function in a particular package.
  = StaticTarget !StaticCallTarget
  | DynamicTarget
  -- ^ The first argument of the import is the name of a function
  --   pointer (an Addr#).  Used when importing a label as "foreign
  --   import ccall "dynamic" ..."
  deriving (Eq, Generic, Show)

data StaticCallTarget =
  StaticCallTarget
    { sourceText :: SourceText -- of the CLabelString. See note [Pragma source text] in BasicTypes
    , byteString :: ByteString -- C-land name of label.
    , functionOrValue :: FunctionOrValue -- allowed in CAPI imports
    } deriving (Eq, Generic, Show)

data FunctionOrValue = IsFunction | IsValue
  deriving( Eq, Generic, Show )

data CCallConv
  = CCallConv
  | CApiConv
  | StdCallConv
  -- Unsupported GHC ones:
  -- | PrimCallConv
  -- | JavaScriptCallConv
  deriving (Eq, Generic, Show)

data Safety
  = PlaySafe            -- Might invoke Haskell GC, or do a call back, or
                        -- switch threads, etc.  So make sure things are
                        -- tidy before the call. Additionally, in the threaded
                        -- RTS we arrange for the external call to be executed
                        -- by a separate OS thread, i.e., _concurrently_ to the
                        -- execution of other Haskell threads.

  | PlayInterruptible   -- Like PlaySafe, but additionally
                        -- the worker thread running this foreign call may
                        -- be unceremoniously killed, so it must be scheduled
                        -- on an unbound thread.

  | PlayRisky           -- None of the above can happen; the call will return
                        -- without interacting with the runtime system at all
  deriving ( Eq, Generic, Show )

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
instance Binary PrimElemRep
instance Binary LitAlt
instance Binary Closure
instance Binary Con
instance Binary CCallSpec
instance Binary CCallTarget
instance Binary CCallConv
instance Binary Safety
instance Binary SourceText
instance Binary FunctionOrValue
instance Binary StaticCallTarget
instance Binary FFIReturnType
instance Binary FFIType
