{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Prana.Types where

import Data.ByteString (ByteString)
import GHC.Generics

data Bind
  = NonRec Var
           Exp
  | Rec [(Var, Exp)]
  deriving (Generic, Show, Ord, Eq)

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
  deriving (Generic, Eq, Show, Ord)

data Var = Var ByteString
  deriving (Generic, Eq, Show, Ord)

data Id = Id ByteString
  deriving (Generic, Eq, Show, Ord)

data Typ = Typ ByteString
  deriving (Generic, Eq, Show, Ord)

data Alt = Alt
  { altCon :: AltCon
  , altVars :: [Var]
  , altExp :: Exp
  } deriving (Generic, Eq, Show, Ord)

data AltCon
  = DataAlt DataCon
  | LitAlt Lit
  | DEFAULT
  deriving (Generic, Eq, Show, Ord)

newtype DataCon = DataCon ByteString
 deriving (Generic, Eq, Show, Ord)

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
  deriving (Generic, Eq, Show, Ord)
