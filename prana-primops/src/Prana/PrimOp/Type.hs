{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Derive the primop type.

module Prana.PrimOp.Type where

import Prana.PrimOp

$derivePrimOpType
