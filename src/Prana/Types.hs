{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Prana.Types where

import           Data.ByteString (ByteString)
import           Data.Data
import           Data.Int
import           GHC.Generics

data Bind =
  Bind
    { bindVar :: !VarId
    , bindExp :: Exp
    }
  deriving (Generic, Data, Typeable, Show, Ord, Eq)

data Exp
  --
  -- Core tree shapes
  --
  = AppE Exp Exp -- ^ Apply a function to an argument.
  | LamE LocalVarId Exp -- ^ A lambda.
  | CaseE Exp VarId Typ [Alt] -- ^ A case analysis.
  | LetE [(LocalVarId, Exp)] Exp -- ^ Let binding of variables.
  --
  -- Constants
  --
  | LitE Lit
  --
  -- Various namespaces variables
  --
  | VarE VarId -- ^ Locally lambda bound or global variable.
  | ConE ConId -- ^ Data constructor.
  | PrimOpE PrimId -- ^ A primitive operation.
  | FFIE FFIId -- ^ A foreign function call.
  | WiredInE WiredId -- ^ Reference to a wired in name.
  --
  -- Type-class infrastructure
  --
  | MethodE MethodId -- ^ A generic method call.
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data ConId = ConId !Int64
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data PrimId = PrimId
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data WiredId = WiredId
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data MethodId = MethodId
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data FFIId = FFIId
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data DictId = DictId
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data VarId = LocalIndex !Int64 | ExportedIndex !Int64
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data LocalVarId = LocalVarId !Int64
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data TyId = TyId
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data Typ = TyConApp TyId [Typ] | OpaqueType ByteString
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data Alt = Alt
  { altCon :: AltCon
  , altVars :: [VarId]
  , altExp :: Exp
  } deriving (Generic, Data, Typeable, Eq, Show, Ord)

data AltCon
  = DataAlt DataCon
  | LitAlt Lit
  | DEFAULT
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

newtype Unique = Unique {unUnique :: Int64}
 deriving (Generic, Data, Typeable, Eq, Show, Ord)

data Strictness = Strict | NonStrict
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data DataCon = DataCon ConId [Strictness]
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data Lit
  = Char Char
  | Str ByteString
  | NullAddr
  | Int Integer
  | Int64Lit Integer
  | Word Integer
  | Word64 Integer
  | Float Rational
  | Double Rational
  | Label
  | Integer Integer
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data ExportedId =
  ExportedId
    { exportedIdPackage :: {-# UNPACK  #-}!ByteString
    , exportedIdModule  :: {-# UNPACK  #-}!ByteString
    , exportedIdName    :: {-# UNPACK  #-}!ByteString
    } deriving (Show, Ord, Eq)

data LocalId =
  LocalId
    { localIdPackage :: {-# UNPACK  #-}!ByteString
    , localIdModule  :: {-# UNPACK  #-}!ByteString
    , localIdName    :: {-# UNPACK  #-}!ByteString
    , localIdUnique  :: {-# UNPACK  #-}!Unique
    } deriving (Show, Ord, Eq)

data ConstrId =
  ConstrId
    { constrIdPackage :: {-# UNPACK  #-}!ByteString
    , constrIdModule  :: {-# UNPACK  #-}!ByteString
    , constrIdName    :: {-# UNPACK  #-}!ByteString
    } deriving (Show, Ord, Eq)
