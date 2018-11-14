-- |

module Prana.Types where

import           Data.ByteString (ByteString)
import           Data.Int
import           Data.Word

data Exp
  = VarE Id
  | LitE Lit
  | AppE Exp Exp
  | LamE Var Exp
  | CaseE Exp Var Typ [Alt]
  deriving (Eq, Show, Ord)

data Var = Var ByteString
  deriving (Eq, Show, Ord)

data Id = Id ByteString
  deriving (Eq, Show, Ord)

data Typ = Typ ByteString
  deriving (Eq, Show, Ord)

data Alt = Alt
  { altCon :: Con
  , altVars :: [Var]
  , altExp :: Exp
  } deriving (Eq, Show, Ord)

data Con = Con
  deriving (Eq, Show, Ord)

data Lit
  = Char Char
  | Str ByteString
  | NullAddr
  | Int Int
  | Int64 Int64
  | Word Word
  | Word64 Word64
  | Float Float
  | Double Double
  | Label
  | Integer Integer
  deriving (Eq, Show, Ord)

data Caf = Caf
