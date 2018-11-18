{-# LANGUAGE LambdaCase #-}

-- |

module Prana.Decode where

import Data.Binary.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import GHC.Real
import Prana.Types

decodeBind :: Get Bind
decodeBind =
  label "decodeBind" $ do
    tag <- getWord8
    case tag of
      0 -> NonRec <$> decodeVar <*> decodeExpr
      1 -> Rec <$> decodeArray ((,) <$> decodeVar <*> decodeExpr)
      _ -> fail ("decodeBind: unknown tag " ++ show tag)

decodeExpr :: Get Exp
decodeExpr =
  label "decodeExpr" $ do
    tag <- getWord8
    case tag of
      0 -> VarE <$> decodeId
      1 -> LitE <$> decodeLit
      2 -> AppE <$> decodeExpr <*> decodeExpr
      3 -> LamE <$> decodeVar <*> decodeExpr
      4 -> LetE <$> decodeBind <*> decodeExpr
      5 ->
        CaseE <$> decodeExpr <*> decodeVar <*> decodeType <*>
        decodeArray decodeAlt
      6 -> CastE <$> decodeExpr
      7 -> TickE <$> decodeExpr
      8 -> TypE <$> decodeType
      9 -> pure CoercionE
      _ -> fail ("decodeExpr: unknown tag " ++ show tag)

decodeLit :: Get Lit
decodeLit =
  label "decodeLit" $ do
    tag <- getWord8
    case tag of
      0 -> Char <$> decodeChar
      1 -> Str <$> decodeByteString
      2 -> pure NullAddr
      3 -> Int <$> decodeInteger
      4 -> Int64 <$> decodeInteger
      5 -> Word <$> decodeInteger
      6 -> Word64 <$> decodeInteger
      7 -> Float <$> ((:%) <$> decodeInteger <*> decodeInteger)
      8 -> Double <$> ((:%) <$> decodeInteger <*> decodeInteger)
      9 -> pure Label
      10 -> Integer <$> decodeInteger
      _ -> fail ("decodeLit: unknown tag " ++ show tag)

decodeAltCon :: Get AltCon
decodeAltCon =
  label "decodeAltCon" $ do
    tag <- getWord8
    case tag of
      0 -> DataAlt <$> decodeDataCon
      1 -> LitAlt <$> decodeLit
      2 -> pure DEFAULT
      _ -> fail ("decodeAltCon: unknown tag " ++ show tag)

decodeInteger :: Get Integer
decodeInteger = label "decodeInteger" $ fmap (read . S8.unpack) decodeByteString

decodeAlt :: Get Alt
decodeAlt =
  label "decodeAlt" $
  Alt <$> decodeAltCon <*> decodeArray decodeVar <*> decodeExpr

decodeType :: Get Typ
decodeType = label "decodeType" $ Typ <$> decodeByteString

decodeVar :: Get Var
decodeVar = label "decodeVar" $ Var <$> decodeUnique

decodeId :: Get Id
decodeId = label "decodeId" $ Id <$> decodeUnique

decodeUnique :: Get Unique
decodeUnique = fmap (Unique . fromIntegral) getInt64le

decodeDataCon :: Get DataCon
decodeDataCon = label "decodeDataCon" $ DataCon <$> decodeUnique

decodeByteString :: Get ByteString
decodeByteString =
  label "decodeByteString" $ do
    len <- getInt64le
    getByteString (fromIntegral len)

decodeChar :: Get Char
decodeChar =
  label "decodeChar" $ do
    i <- getInt64le
    pure (toEnum (fromIntegral i))

decodeArray :: Get a -> Get [a]
decodeArray v =
  label "decodeArray" $ do
    i <- getInt64le
    sequence (replicate (fromIntegral i) v)
