{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Prana.Types where

import Data.ByteString (ByteString)
import Data.Data
import GHC.Generics



data Bind
  = NonRec Id
           Exp
  | Rec [(Id, Exp)]
  deriving (Generic, Data, Typeable, Show, Ord, Eq)

data Exp
  = VarE Id
  | LitE Lit
  | AppE Exp Exp
  | LamE Id Exp
  | CaseE Exp Id Typ [Alt]
  | TypE Typ
  | CoercionE
  | LetE Bind Exp
  | CastE Exp
  | TickE Exp
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

-- Use this encoding:
--
-- <stable-name> <unique>
--
-- where
--
-- stable-name = <package> <module> <ident>
--
-- and unique is a per-GHC-run unique number, which for our purposes
-- means "a package-level unique number".
data Id = Id
  { idStableName :: {-# UNPACK #-}!ByteString
  , idUnique :: {-# UNPACK #-}!Unique
  , idCategory :: !Cat
  } deriving (Generic, Data, Typeable, Eq, Show, Ord)

data Cat
  = ValCat
  | DataCat
  | ClassCat
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data Typ = Typ ByteString
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data Alt = Alt
  { altCon :: AltCon
  , altVars :: [Id]
  , altExp :: Exp
  } deriving (Generic, Data, Typeable, Eq, Show, Ord)

data AltCon
  = DataAlt DataCon
  | LitAlt Lit
  | DEFAULT
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

newtype Unique = Unique Int
 deriving (Generic, Data, Typeable, Eq, Show, Ord)

newtype DataCon = DataCon Unique
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
