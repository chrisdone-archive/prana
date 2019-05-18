{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Derive the primop type.

module Prana.PrimOp.Type where

import Data.Flat
import GHC.Generics
import Prana.PrimOp

$derivePrimOpType
