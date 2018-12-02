{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

-- |

module Prana.View (LitV(StrP)) where

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as L
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as S
import           Data.Int
import           Data.Word

-- " A view on a Lit.
-- data Lit = Str ByteString | ...
newtype LitV = LitV ByteString

{-# COMPLETE StrP #-}

-- | The @Str ByteString@ case.
pattern StrP :: ByteString -> LitV
pattern StrP s <- (readStrP -> Just s)

-- | Check for the Str constructor, then access.
readStrP :: LitV -> Maybe ByteString
readStrP (LitV s) =
  if S.unsafeIndex s 0 == 1
    then Just (readByteString (S.unsafeDrop 1 s))
    else Nothing
{-# INLINE readStrP #-}

{-# INLINE readByteString #-}
-- | Read a ByteString from the encoding.
readByteString :: ByteString -> ByteString
readByteString bs = S.unsafeTake (readInt bs) (S.unsafeDrop 8 bs)

{-# INLINE readInt #-}
readInt :: ByteString -> Int
readInt = fromIntegral . readInt64

{-# INLINE readInt64 #-}
readInt64 :: ByteString -> Int64
readInt64 = fromIntegral . readWord64

{-# INLINE readWord64 #-}
readWord64 :: ByteString -> Word64
readWord64 s =
  x7 `unsafeShiftL` 56 .|. x6 `unsafeShiftL` 48 .|. x5 `unsafeShiftL` 40 .|.
  x4 `unsafeShiftL` 32 .|.
  x3 `unsafeShiftL` 24 .|.
  x2 `unsafeShiftL` 16 .|.
  x1 `unsafeShiftL` 8 .|.
  x0
  where
    x0 = fromIntegral (S.unsafeIndex s 0) :: Word64
    x1 = fromIntegral (S.unsafeIndex s 1) :: Word64
    x2 = fromIntegral (S.unsafeIndex s 2) :: Word64
    x3 = fromIntegral (S.unsafeIndex s 3) :: Word64
    x4 = fromIntegral (S.unsafeIndex s 4) :: Word64
    x5 = fromIntegral (S.unsafeIndex s 5) :: Word64
    x6 = fromIntegral (S.unsafeIndex s 6) :: Word64
    x7 = fromIntegral (S.unsafeIndex s 7) :: Word64
