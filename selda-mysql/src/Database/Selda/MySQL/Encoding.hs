{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Selda.MySQL.Encoding
  ( getTextRowSql, litToMySQL, mySQLToSql,
    unChunk, toSqlTypeRep, putTextFieldSql )
where
import Database.MySQL.Base
import Database.Selda.Backend
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Blaze.Text                         as Textual
import Data.Binary
import Data.Binary.Put
import Data.Binary.Parser
import Data.Time
import Data.Maybe
import Data.Text
import Control.Monad
import Data.Text.Encoding as T
import qualified Data.ByteString.Lex.Fractional     as LexFrac
import qualified Data.ByteString.Lex.Integral       as LexInt
import           Data.ByteString.Builder.Scientific (FPFormat (..),
                                                     formatScientificBuilder)
import qualified Data.ByteString.Unsafe as B
import Database.MySQL.Protocol.Escape

-- | Text protocol decoder
getTextFieldSql :: ColumnDef -> Get SqlValue
getTextFieldSql f
    | t == mySQLTypeNull            = pure SqlNull
    | t == mySQLTypeDecimal ||
      t == mySQLTypeNewDecimal      = SqlBlob <$> getLenEncBytes
    | t == mySQLTypeTiny  ||
      t == mySQLTypeShort ||
      t == mySQLTypeLong  ||
      t == mySQLTypeInt24 ||
      t == mySQLTypeLongLong        = feedLenEncBytes t SqlInt intLexer
    | t == mySQLTypeFloat           = feedLenEncBytes t SqlFloat fracLexer
    | t == mySQLTypeDouble          = feedLenEncBytes t SqlFloat fracLexer
    | t == mySQLTypeYear            = feedLenEncBytes t SqlInt intLexer
    | t == mySQLTypeTimestamp ||
      t == mySQLTypeTimestamp2      = SqlString . decodeUtf8 <$> getLenEncBytes
    | t == mySQLTypeDateTime  ||
      t == mySQLTypeDateTime2 ||
      t == mySQLTypeDate      ||
      t == mySQLTypeNewDate   ||
      t == mySQLTypeTime ||
      t == mySQLTypeTime2           = SqlString . decodeUtf8 <$> getLenEncBytes
    | t == mySQLTypeGeometry        = SqlBlob <$> getLenEncBytes
    | t == mySQLTypeVarChar    ||
      t == mySQLTypeEnum       ||
      t == mySQLTypeSet        ||
      t == mySQLTypeTinyBlob   ||
      t == mySQLTypeMediumBlob ||
      t == mySQLTypeLongBlob   ||
      t == mySQLTypeBlob       ||
      t == mySQLTypeVarString  ||
      t == mySQLTypeString     =
        if isText then SqlString . decodeUtf8 <$> getLenEncBytes
        else SqlBlob <$> getLenEncBytes

    | t == mySQLTypeBit             = SqlBlob <$> getLenEncBytes

    | otherwise                     = fail $ "Database.MySQL.Protocol.MySQLValue: missing text decoder for " ++ show t
  where
    t = columnType f
    --isUnsigned = flagUnsigned (columnFlags f)
    isText = columnCharSet f /= 63
    intLexer bs = fst <$> LexInt.readSigned LexInt.readDecimal bs
    fracLexer bs = fst <$> LexFrac.readSigned LexFrac.readDecimal bs

getTextRowSql :: [ColumnDef] -> Get [SqlValue]
getTextRowSql fs = forM fs $ \ f -> do
    p <- peek
    if p == 0xFB
    then skipN 1 >> return SqlNull
    else getTextFieldSql f
{-# INLINE getTextRowSql #-}


feedLenEncBytes :: FieldType -> (t -> b) -> (BC.ByteString -> Maybe t) -> Get b
feedLenEncBytes typ con parser = do
    bs <- getLenEncBytes
    case parser bs of
        Just v -> return (con v)
        Nothing -> fail $ "Database.MySQL.Protocol.MySQLValue: parsing " ++
                   show typ ++ " failed, input: " ++ BC.unpack bs
{-# INLINE feedLenEncBytes #-}



-- | Convert a lazy bytestring to a strict one.
--   Avoids the copying overhead of 'LBS.toStrict' when there's only a single
--   chunk, which should always be the case when serializing single parameters.
unChunk :: LBS.ByteString -> BS.ByteString
unChunk bs =
  case LBS.toChunks bs of
    [bs'] -> bs'
    bss   -> BS.concat bss

toSqlTypeRep :: Text -> Either Text SqlTypeRep
toSqlTypeRep "bit" = Right TInt
toSqlTypeRep "int" = Right TInt
toSqlTypeRep "tinyint" = Right TInt
toSqlTypeRep "bool" = Right TInt
toSqlTypeRep "boolean" = Right TInt
toSqlTypeRep "smallint" = Right TInt
toSqlTypeRep "mediumint" = Right TInt
toSqlTypeRep "bigint" = Right TBlob
toSqlTypeRep "integer" = Right TInt
toSqlTypeRep "decimal" = Right TBlob
toSqlTypeRep "dec" = Right TText
toSqlTypeRep "fixed" = Right TBlob
toSqlTypeRep "float" = Right TFloat
toSqlTypeRep "double" = Right TFloat
toSqlTypeRep "real" = Right TFloat
toSqlTypeRep "date" = Right TDate
toSqlTypeRep "datetime" = Right TDateTime
toSqlTypeRep "timestamp" = Right TTime
toSqlTypeRep "time" = Right TTime
toSqlTypeRep "year" = Right TInt
toSqlTypeRep "var" = Right TText
toSqlTypeRep "varchar" = Right TText
toSqlTypeRep "binary" = Right TBlob
toSqlTypeRep "varbinary" = Right TBlob
toSqlTypeRep "tinyblob" = Right TBlob
toSqlTypeRep "tinytext" = Right TText
toSqlTypeRep "blob" = Right TBlob
toSqlTypeRep "text" = Right TText
toSqlTypeRep "mediumblob" = Right TText
toSqlTypeRep "mediumtext" = Right TText
toSqlTypeRep "longblob" = Right TBlob
toSqlTypeRep "longtext" = Right TText
toSqlTypeRep "enum" = Right TText
toSqlTypeRep t = Left t

-- | Text protocol encoder
putTextFieldSql :: Lit a -> Put
putTextFieldSql (LText t) = putInQuotes $ putByteString $
                            encodeUtf8 $ escapeText t
putTextFieldSql (LInt i)  = putBuilder (Textual.integral i)
putTextFieldSql (LDouble d) = putBuilder (Textual.double d)
putTextFieldSql (LBool b) = if b then putBuilder "0"
                            else putBuilder "1"
                                 -- no need t escape dates
putTextFieldSql (LDateTime t) = putInQuotes $ putByteString $
                                encodeUtf8 t 
putTextFieldSql (LDate t) = putInQuotes $ putByteString $
                            encodeUtf8 t
putTextFieldSql (LTime t) = putInQuotes $ putByteString $
                            encodeUtf8 t 
putTextFieldSql (LJust a) = putTextFieldSql a
putTextFieldSql (LBlob b) = putByteString $ escapeBytes $ b
putTextFieldSql LNull     = putBuilder "NULL"
putTextFieldSql (LCustom a) = putTextFieldSql a

putInQuotes :: Put -> Put
putInQuotes p = putCharUtf8 '\'' >> p >> putCharUtf8 '\''
{-# INLINE putInQuotes #-}

litToMySQL :: Lit a -> MySQLValue
litToMySQL (LText t) = MySQLText t
litToMySQL (LInt i) = MySQLInt64 $ fromIntegral i
litToMySQL (LDouble d) = MySQLDouble d
litToMySQL (LBool b) = MySQLInt8 $ if b then 1 else 0
litToMySQL (LDateTime d) = let bs = encodeUtf8 d
                           in MySQLDateTime $ LocalTime (dateParser bs) $
                              timeParser $ B.unsafeDrop 11 bs
litToMySQL (LDate d) = MySQLDate $ dateParser $ encodeUtf8 d
litToMySQL (LTime d) = MySQLTime 0 $ timeParser $ encodeUtf8 d
litToMySQL (LJust a) = litToMySQL a
litToMySQL (LBlob b) = MySQLBytes b
litToMySQL LNull = MySQLNull
litToMySQL (LCustom a) = litToMySQL a

dateParser :: BC.ByteString -> Day
dateParser bs = fromMaybe (error $ "BUG: Illegal datestring:"
                           ++ BC.unpack bs) $ do
  (yyyy, rest) <- LexInt.readDecimal bs
  (mm, rest') <- LexInt.readDecimal (B.unsafeTail rest)
  (dd, _) <- LexInt.readDecimal (B.unsafeTail rest')
  return (fromGregorian yyyy mm dd)

timeParser :: BC.ByteString -> TimeOfDay
timeParser bs = fromMaybe (error $ "BUG2: Illegal timestring"
                           ++ BC.unpack bs) $ do
  (hh, rest) <- LexInt.readDecimal bs
  (mm, rest') <- LexInt.readDecimal (B.unsafeTail rest)
  (ss, _) <- LexFrac.readDecimal (B.unsafeTail rest')
  return (TimeOfDay hh mm ss)

mySQLToSql :: MySQLValue -> SqlValue
mySQLToSql (MySQLDecimal    n) = SqlBlob $ LBS.toStrict $ toLazyByteString $ 
                                 formatScientificBuilder Fixed Nothing n
mySQLToSql (MySQLInt8U      n) = SqlInt $ fromIntegral n
mySQLToSql (MySQLInt8       n) = SqlInt $ fromIntegral n
mySQLToSql (MySQLInt16U     n) = SqlInt $ fromIntegral n
mySQLToSql (MySQLInt16      n) = SqlInt $ fromIntegral n
mySQLToSql (MySQLInt32U     n) = SqlInt $ fromIntegral n
mySQLToSql (MySQLInt32      n) = SqlInt $ fromIntegral n
mySQLToSql (MySQLInt64U     n) = SqlInt $ fromIntegral n
mySQLToSql (MySQLInt64      n) = SqlInt $ fromIntegral n
mySQLToSql (MySQLFloat      x) = SqlFloat $ realToFrac x
mySQLToSql (MySQLDouble     x) = SqlFloat x
mySQLToSql (MySQLYear       n) = SqlInt $ fromIntegral n
mySQLToSql (MySQLDateTime  dt) = SqlString $ pack $
                                 formatTime defaultTimeLocale
                                 "%F %T%Q" dt
mySQLToSql (MySQLTimeStamp dt) = SqlString $ pack $
                                 formatTime defaultTimeLocale "%F %T%Q" dt
mySQLToSql (MySQLDate       d) = SqlString $ pack $
                                 formatTime defaultTimeLocale "%F" d
mySQLToSql (MySQLTime _sign t) = SqlString $ pack $
                                 formatTime defaultTimeLocale "%T%Q" t
                                      -- this works even for hour > 24
mySQLToSql (MySQLGeometry  bs) = SqlBlob bs
mySQLToSql (MySQLBytes     bs) = SqlBlob bs
mySQLToSql (MySQLText       t) = SqlString t
mySQLToSql (MySQLBit        b) = SqlBlob $ unChunk $ runPut $ putWord64be b
mySQLToSql MySQLNull           = SqlNull
