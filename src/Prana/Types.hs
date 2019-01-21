{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Prana.Types where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Builder as L
import           Data.Data
import           GHC.Generics

class Pretty a where
  pretty :: a -> L.Builder

data Bind
  = NonRec VarId
           Exp
  | Rec [(VarId, Exp)]
  deriving (Generic, Data, Typeable, Show, Ord, Eq)

data Exp
  --
  -- Core tree shapes
  --
  = AppE Exp Exp -- ^ Apply a function to an argument.
  | LamE VarId Exp -- ^ A lambda.
  | CaseE Exp VarId Typ [Alt] -- ^ A case analysis.
  | LetE Bind Exp -- ^ Let binding of variables.
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
  | DictE DictId -- ^ A dictionary passed to a method.
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data ConId = ConId
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

data VarId = LocalIndex !Int | ExportedIndex !Int
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data Cat
  = ValCat
  | DataCat
  | ClassCat
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

newtype Unique = Unique Int
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
  | Int64 Integer
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
    , localIdUnique  :: {-# UNPACK  #-}!Int
    } deriving (Show, Ord, Eq)
