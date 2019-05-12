{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -fno-warn-orphans #-}

-- | A wrapper module around the genprimops code, providing more
-- structure appropriate for prana to consume.

module Prana.Primops
  ( readPrimSpecs
  , PrimOp(..)
  , Category(..)
  , PrimType(..)
  ) where

import           Control.DeepSeq
import           Control.Exception
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           GHC.Generics
import           Language.Haskell.TH.Lift
import qualified Parser
import qualified Syntax

-- Orphan: remove when NonEmpty gets a Lift instance, if ever.
deriving instance Lift l => Lift (NonEmpty l)

data PrimType
  = FunPrimType !PrimType !PrimType
  | UnboxedTupleType !(NonEmpty SimpleType)
  | SimpleType !SimpleType
  deriving (Show, Eq, Generic, Lift)
instance NFData PrimType

-- I think everything up to mutable variables can be derived.
-- Anything with a `(a -> b)` in it probably needs manual work.

data SimpleType
  = AtmoicType !String
  | ParametricType !String !(NonEmpty String)
  deriving (Show, Eq, Generic, Lift)
instance NFData SimpleType

data PrimOp =
  PrimOp
    { primOpCons :: String
    , primOpName :: String
    , primOpType :: PrimType
    , primOpCategory :: Category
    , primOpDescription :: String
    }
  deriving (Show, Eq , Generic, Lift)
instance NFData PrimOp

data Category
  = DyadicCategory
  | MonadicCategory
  | CompareCategory
  | GeneralCategory
  deriving (Show, Eq, Ord, Enum, Generic, Lift)
instance NFData Category

readPrimSpecs :: FilePath -> IO [PrimOp]
readPrimSpecs fp = do
  string <- readFile fp
  case Parser.parse string of
    Left err -> error err
    Right (Syntax.Info _ specs) ->
      evaluate
        (force (mapMaybe
                  (\case
                     Syntax.PrimOpSpec {cons,name,ty,cat,desc} ->
                       Just
                         (PrimOp -- TODO: Use opts field.
                            { primOpCons = cons
                            , primOpName = name
                            , primOpType = toPrimType ty
                            , primOpCategory = toCategory cat
                            , primOpDescription = desc
                            })
                     _ -> Nothing)
                  specs))

toPrimType :: Syntax.Ty -> PrimType
toPrimType = go
  where go = undefined

toCategory :: Syntax.Category -> Category
toCategory =
  \case
    Syntax.Dyadic -> DyadicCategory
    Syntax.Monadic -> MonadicCategory
    Syntax.Compare -> CompareCategory
    Syntax.GenPrimOp -> GeneralCategory
