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

newtype LitV = LitV ByteString

{-# COMPLETE StrP #-}

pattern StrP :: ByteString -> LitV
pattern StrP s <- (readStrP -> Just s) where
  StrP s = LitV (builderBS (tag 1 <> encodeByteString s))

readStrP :: LitV -> Maybe ByteString
readStrP (LitV s) =
  if S.unsafeIndex s 0 == 1
    then Just (readByteString (S.unsafeDrop 1 s))
    else Nothing

builderBS :: L.Builder -> ByteString
builderBS = L.toStrict . L.toLazyByteString

tag :: Word8 -> L.Builder
tag = L.word8

encodeByteString :: ByteString -> L.Builder
encodeByteString x =
  L.int64LE (fromIntegral (S.length x)) <>
  L.byteString x

{-# INLINE readByteString #-}
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
