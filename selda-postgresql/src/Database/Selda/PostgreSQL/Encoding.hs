{-# LANGUAGE GADTs, BangPatterns, OverloadedStrings, CPP #-}
-- | Encoding/decoding for PostgreSQL.
module Database.Selda.PostgreSQL.Encoding
  ( toSqlValue, fromSqlValue, fromSqlType, readInt, readBool
  ) where
#ifdef __HASTE__

toSqlValue, fromSqlValue, fromSqlType, readInt, readBool :: a
toSqlValue = undefined
fromSqlValue = undefined
fromSqlType = undefined
readInt = undefined
readBool = undefined

#else

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Time (utc, localToUTCTimeOfDay)
import Database.PostgreSQL.LibPQ (Oid (..), Format (Binary))
import Database.Selda.Backend
import PostgreSQL.Binary.Encoding as Enc
import PostgreSQL.Binary.Decoding as Dec
import qualified Data.UUID.Types as UUID (toByteString)
import Data.Int (Int16, Int32, Int64)

-- | OIDs for all types used by Selda.
blobType, boolType, intType, int32Type, int16Type, textType, doubleType,
  dateType, timeType, timestampType, nameType, varcharType, uuidType :: Oid
boolType      = Oid 16
intType       = Oid 20
int32Type     = Oid 23
int16Type     = Oid 21
textType      = Oid 25
nameType      = Oid 19
doubleType    = Oid 701
dateType      = Oid 1082
timeType      = Oid 1266
timestampType = Oid 1184
blobType      = Oid 17
varcharType   = Oid 1043
uuidType      = Oid 2950

bytes :: Enc.Encoding -> BS.ByteString
bytes = Enc.encodingBytes

-- | Convert a parameter into an postgres parameter triple.
fromSqlValue :: Lit a -> Maybe (Oid, BS.ByteString, Format)
fromSqlValue (LBool b)     = Just (boolType, bytes $ Enc.bool b, Binary)
fromSqlValue (LInt n)      = Just ( intType
                                  , bytes $ Enc.int8_int64 $ fromIntegral n
                                  , Binary)
fromSqlValue (LDouble f)   = Just (doubleType, bytes $ Enc.float8 f, Binary)
fromSqlValue (LText s)     = Just (textType, bytes $ Enc.text_strict s, Binary)
fromSqlValue (LDateTime t) = Just ( timestampType
                                  , bytes $ Enc.timestamptz_int t
                                  , Binary)
fromSqlValue (LTime t)     = Just (timeType, bytes $ Enc.timetz_int (t, utc), Binary)
fromSqlValue (LDate d)     = Just (dateType, bytes $ Enc.date d, Binary)
fromSqlValue (LUUID x)     = Just (uuidType, bytes $ Enc.uuid x, Binary)
fromSqlValue (LBlob b)     = Just (blobType, bytes $ Enc.bytea_strict b, Binary)
fromSqlValue (LNull)       = Nothing
fromSqlValue (LJust x)     = fromSqlValue x
fromSqlValue (LCustom l)   = fromSqlValue l

-- | Get the corresponding OID for an SQL type representation.
fromSqlType :: SqlTypeRep -> Oid
fromSqlType TBool     = boolType
fromSqlType TInt      = intType
fromSqlType TFloat    = doubleType
fromSqlType TText     = textType
fromSqlType TDateTime = timestampType
fromSqlType TDate     = dateType
fromSqlType TTime     = timeType
fromSqlType TBlob     = blobType
fromSqlType TRowID    = intType
fromSqlType TUUID     = uuidType

-- | Convert the given postgres return value and type to an @SqlValue@.
toSqlValue :: Oid -> BS.ByteString -> SqlValue
toSqlValue t val
  | t == boolType      = SqlBool    $ parse Dec.bool val
  | t == intType       = SqlInt     $ fromIntegral $ parse (Dec.int :: Value Int64) val
  | t == int32Type     = SqlInt     $ fromIntegral $ parse (Dec.int :: Value Int32) val
  | t == int16Type     = SqlInt     $ fromIntegral $ parse (Dec.int :: Value Int16) val
  | t == doubleType    = SqlFloat   $ parse Dec.float8 val
  | t == blobType      = SqlBlob    $ parse Dec.bytea_strict val
  | t == uuidType      = SqlBlob    $ toBS $ parse Dec.uuid val
  | t == timestampType = SqlUTCTime $ parse parseTimestamp val
  | t == timeType      = SqlTime    $ toTime $ parse parseTime val
  | t == dateType      = SqlDate    $ parse Dec.date val
  | t `elem` textish   = SqlString  $ parse Dec.text_strict val
  | otherwise          = error $ "BUG: result with unknown type oid: " ++ show t
  where
    parseTimestamp = Dec.timestamptz_int <|> Dec.timestamptz_float
    parseTime = Dec.timetz_int <|> Dec.timetz_float
    toTime (tod, tz) = snd $ localToUTCTimeOfDay tz tod
    toBS = LBS.toStrict . UUID.toByteString
    textish = [textType, nameType, varcharType]

parse :: Value a -> BS.ByteString -> a
parse p x =
  case valueParser p x of
    Right x' -> x'
    Left _   -> error "unable to decode value"

-- | Read an Int from a binary encoded pgint8.
readInt :: BS.ByteString -> Int
readInt = fromIntegral . parse (Dec.int :: Value Int64)

readBool :: T.Text -> Bool
readBool = go . T.map toLower
  where
    go "f"     = False
    go "0"     = False
    go "false" = False
    go "n"     = False
    go "no"    = False
    go "off"   = False
    go _       = True

#endif
