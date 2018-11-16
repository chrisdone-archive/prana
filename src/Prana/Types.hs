{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Prana.Types where

import Data.ByteString (ByteString)
import Data.Int
import Data.Word
import GHC.Generics
import Persist (Persist(..),Get,getBytes)

data Bind
  = NonRec Var
           Exp
  | Rec [(Var, Exp)]
  deriving (Generic, Show, Ord, Eq)
instance Persist Bind

instance Persist Exp
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
  deriving (Generic, Eq, Show, Ord)

instance Persist Var
data Var = Var ByteString
  deriving (Generic, Eq, Show, Ord)

instance Persist Id
data Id = Id ByteString
  deriving (Generic, Eq, Show, Ord)

instance Persist Typ
data Typ = Typ ByteString
  deriving (Generic, Eq, Show, Ord)

instance Persist Alt
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
instance Persist AltCon

instance Persist DataCon
newtype DataCon = DataCon ByteString
 deriving (Generic, Eq, Show, Ord)

instance Persist Lit
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
