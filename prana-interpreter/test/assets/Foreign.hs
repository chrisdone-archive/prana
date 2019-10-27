{-# LANGUAGE NoImplicitPrelude #-}

-- | Demonstrate various use of the FFI.

module Foreign where

import Foreign.C

foreign import ccall "math.h sin"
   sin :: CDouble -> CDouble

it :: CDouble
it = sin 1
