{-# LANGUAGE NoImplicitPrelude #-}

-- | Demonstrate various use of the FFI.

module Atan2 where

import Foreign.C

foreign import ccall "math.h atan2"
   atan2 :: CDouble -> CDouble -> CDouble

it :: CDouble
it = atan2 (-10) (10)
