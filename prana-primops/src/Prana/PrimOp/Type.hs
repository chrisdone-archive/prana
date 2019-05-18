{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Derive the primop type.

module Prana.PrimOp.Type where

import Data.Binary
import Prana.PrimOp

$derivePrimOpType

instance Binary PrimOp
