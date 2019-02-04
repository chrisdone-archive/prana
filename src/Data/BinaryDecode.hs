{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
-- |

module Data.BinaryDecode
  ( decodeArray
  , decodeInt
  , decodeWord16
  , decodeShortByteString
  , decodeByteString
  , runDecode
  ) where

import           Control.Monad
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as S
import           Data.Int
import           Data.Word

newtype Decode a =
  Decode
    { runDecode :: ByteString -> (a, ByteString)
    }
  deriving (Functor)
instance Applicative Decode where
  (<*>) = ap
  pure = return
  {-# INLINE (<*>) #-}
instance Monad Decode where
  {-# INLINE return #-}
  return v = Decode (\s -> (v,s))
  {-# INLINE (>>=) #-}
  Decode g >>= f =
    Decode (\bs -> let (!v, !s) = g bs
                       Decode !k = f v
                   in k s)

{-# INLINE decodeArray #-}
decodeArray :: Decode a -> Decode [a]
decodeArray d = do
  len <- decodeInt
  mapM (const d) [1 .. len]

{-# INLINE decodeWord64 #-}
decodeWord64 :: Decode Word64
decodeWord64 = Decode (\bs -> (readWord64 bs, S.unsafeDrop 8 bs))

{-# INLINE decodeInt64 #-}
decodeInt64 :: Decode Int64
decodeInt64 = fmap fromIntegral decodeWord64

{-# INLINE decodeInt #-}
decodeInt :: Decode Int
decodeInt = fmap fromIntegral decodeInt64

{-# INLINE decodeWord16 #-}
decodeWord16 :: Decode Word16
decodeWord16 = Decode (\bs -> (readWord16le bs, S.unsafeDrop 2 bs))

{-# INLINE decodeByteString #-}
decodeByteString :: Decode ByteString
decodeByteString = do
  len <- decodeInt
  str <- decodeBytes (fromIntegral len)
  pure str

{-# INLINE decodeShortByteString #-}
decodeShortByteString :: Decode ByteString
decodeShortByteString = do
  len <- decodeWord16
  str <- decodeBytes (fromIntegral len)
  pure str

{-# INLINE decodeBytes #-}
decodeBytes :: Int -> Decode ByteString
decodeBytes i = Decode (\bs -> (S.unsafeTake i bs, S.unsafeDrop i bs))

--------------------------------------------------------------------------------
-- Low-evel readers

readWord16le :: ByteString -> Word16
readWord16le = \s ->
              (fromIntegral (s `S.unsafeIndex` 1) `unsafeShiftL` 8) .|.
              (fromIntegral (s `S.unsafeIndex` 0) )

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
