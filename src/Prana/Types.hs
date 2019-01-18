{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Prana.Types where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Builder as L
import           Data.Data
import           Data.List
import           Data.String
import           GHC.Generics

class Pretty a where
  pretty :: a -> L.Builder

data Bind
  = NonRec VarId
           Exp
  | Rec [(VarId, Exp)]
  deriving (Generic, Data, Typeable, Show, Ord, Eq)

instance Pretty Bind where
  pretty =
    \case
      NonRec _i e -> {-pretty i-}"TODO" <> " = " <> pretty e
      Rec pairs ->
        mconcat (intersperse "; " (map (pretty . uncurry NonRec) pairs))

data Exp
  = VarE VarId
  | ConE ConId
  | PrimOpE PrimId
  | SysE SysId
  | WiredInE WiredId
  | MethodE MethodId
  | FFIE FFIId
  | DictE DictId
  | LitE Lit
  | AppE Exp Exp
  | LamE Bool VarId Exp
  | CaseE Exp VarId Typ [Alt]
  | TypE Typ
  | CoercionE
  | LetE Bind Exp
  | CastE Exp
  | TickE Exp
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

instance Pretty Exp where
  pretty =
    \case
      VarE _i -> "TODO:VarE" {-pretty i-}
      ConE _i -> "TODO:ConE" {-pretty i-}
      LitE l -> pretty l
      AppE f x -> "(" <> pretty f <> " " <> pretty x <> ")"
      LamE ty _i e ->
        "(\\" <>
        (if ty
           then "@"
           else "") <>
        "TODO"{-pretty i-} <>
        " -> " <>
        pretty e <>
        ")"
      LetE b e -> "(let { " <> pretty b <> " } in " <> pretty e <> ")"
      CastE e -> pretty e
      TypE ty ->  pretty ty
      CoercionE {} -> "Coercion"
      TickE e -> pretty e
      CaseE e _ty _i alts ->
        "(case " <> pretty e <> " of {" <>
        mconcat (intersperse ";" (map pretty alts)) <>
        " })"

data ConId = ConId
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data PrimId = PrimId
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

data SysId = SysId
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

instance Pretty Typ where
  pretty =
    \case
      (OpaqueType ty) -> "Type[" <> L.byteString ty <> "]"
      (TyConApp _i tys) ->
        "TyConApp(" <> "TODO"{-pretty i-} <> ")[" <>
        mconcat (intersperse ", " (map pretty tys)) <>
        "]"

data Alt = Alt
  { altCon :: AltCon
  , altVars :: [VarId]
  , altExp :: Exp
  } deriving (Generic, Data, Typeable, Eq, Show, Ord)

instance Pretty Alt where
  pretty (Alt con ids e) =
    pretty con <> "(" <> mconcat (intersperse " " (map (const "TODO"{-pretty-}) ids)) <> ") -> " <>
    pretty e

data AltCon
  = DataAlt DataCon
  | LitAlt Lit
  | DEFAULT
  deriving (Generic, Data, Typeable, Eq, Show, Ord)

instance Pretty AltCon where
  pretty =
    \case
      DataAlt (DataCon _i strictness) -> "TODO"{-pretty i-} <> "[" <> fromString (show strictness) <> "]"
      LitAlt l -> pretty l
      DEFAULT -> "_"

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

instance Pretty Lit where
  pretty = \case
              Char c -> render c
              Str c -> render c
              NullAddr -> "nullAddr#"
              Int c -> render c
              Int64 c -> render c
              Word c -> render c
              Word64 c -> render c
              Float c -> render c
              Double c -> render c
              Label -> "Label"
              Integer c -> render c
    where render :: Show a => a -> L.Builder
          render = L.byteString . S8.pack . show

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
