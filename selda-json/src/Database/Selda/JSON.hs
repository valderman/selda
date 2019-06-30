{-# LANGUAGE GADTs, OverloadedStrings #-}
module Database.Selda.JSON (JSONBackend (..)) where
import Database.Selda (Text, Col, Inner)
import Database.Selda.Backend
import Database.Selda.Unsafe (sink, sink2)
import Data.Aeson (Value (Null), encode, decode', FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Lazy as BSL (ByteString, fromStrict, toStrict)
import Data.Text.Encoding (encodeUtf8)

class JSONValue a
instance JSONValue Value
instance JSONValue a => JSONValue (Maybe a)

-- | Any backend that supports JSON lookups in queries.
class JSONBackend b where
  -- | Look up the given key in the given JSON column.
  (~>) :: JSONValue a => Col b a -> Col b Text -> Col b (Maybe Value)
  infixl 8 ~>

  -- | Convert the given JSON column to plain text.
  jsonToText :: Col b Value -> Col b Text

instance JSONBackend b => JSONBackend (Inner b) where
  (~>) = sink2 (~>)
  jsonToText = sink jsonToText

decodeError :: Show a => a -> b
decodeError x = error $ "fromSql: json column with invalid json: " ++ show x

typeError :: Show a => a -> b
typeError x = error $ "fromSql: json column with non-text value: " ++ show x

textToLazyBS :: Text -> BSL.ByteString
textToLazyBS = BSL.fromStrict . encodeUtf8

instance SqlType Value where
  mkLit = LCustom TJSON . LBlob . BSL.toStrict . encode
  sqlType _ = TJSON
  defaultValue = mkLit Null
  fromSql (SqlBlob t)   = maybe (decodeError t) id (decode' $ BSL.fromStrict t)
  fromSql (SqlString t) = maybe (decodeError t) id (decode' $ textToLazyBS t)
  fromSql x             = typeError x

instance FromJSON RowID where
  parseJSON = fmap toRowId . parseJSON
instance ToJSON RowID where
  toJSON = toJSON . fromRowId

instance FromJSON (ID a) where
  parseJSON = fmap toId . parseJSON
instance ToJSON (ID a) where
  toJSON = toJSON . fromId
