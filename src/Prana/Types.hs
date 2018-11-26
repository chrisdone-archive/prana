{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Prana.Types where

import Data.ByteString (ByteString)
import Data.Data
import GHC.Generics

data Bind
  = NonRec Var
           Exp
  | Rec [(Var, Exp)]
  deriving (Generic, Data, Typeable, Show, Ord, Eq)

data Exp
  = VarE Id
  | LitE Lit
  | AppE Exp Exp
  | LamE Var Exp
  | CaseE Exp Var Typ [Alt]
  | TypE Typ
  | CoercionE
  | LetE Bind Exp
  | CastE Exp
  | TickE Exp
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

-- An easy way to uniquify everything. Use parts:
--
-- 1. Package + module + ident.
-- 2. Unique from GHC run.
--
-- So:
--
-- * When we do a GLOBAL lookup, we just lookup by package + module +
-- ident.
--
-- * When we do a LOCAL lookup, we use the complete composed
-- package+module+ident+unique.
--

data Var = Var Unique
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data Id = Id Unique
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data Typ = Typ ByteString
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data Alt = Alt
  { altCon :: AltCon
  , altVars :: [Var]
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
