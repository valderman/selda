{-# LANGUAGE OverloadedStrings #-}
module Database.Selda.SQL.Print.Config (PPConfig (..), defPPConfig) where
import Data.Text (Text)
import qualified Data.Text as T
import Database.Selda.SqlType
import Database.Selda.Table

-- | Backend-specific configuration for the SQL pretty-printer.
data PPConfig = PPConfig
  { -- | The SQL type name of the given type.
    ppType :: SqlTypeRep -> Text

    -- | Parameter placeholder for the @n@th parameter.
  , ppPlaceholder :: Int -> Text

    -- | List of column attributes, such as 
  , ppColAttrs :: [ColAttr] -> Text

    -- | The value used for the next value for an auto-incrementing column.
    --   For instance, @DEFAULT@ for PostgreSQL, and @NULL@ for SQLite.
  , ppAutoIncInsert :: Text
  }

-- | Default settings for pretty-printing.
--   Geared towards SQLite.
defPPConfig :: PPConfig
defPPConfig = PPConfig
    { ppType = defType
    , ppPlaceholder = T.cons '$' . T.pack . show
    , ppColAttrs = T.unwords . map defColAttr
    , ppAutoIncInsert = "NULL"
    }

-- | Default compilation for SQL types.
defType :: SqlTypeRep -> Text
defType TText     = "TEXT"
defType TRowID    = "INTEGER"
defType TInt      = "INT"
defType TFloat    = "DOUBLE"
defType TBool     = "INT"
defType TDateTime = "DATETIME"
defType TDate     = "DATE"
defType TTime     = "TIME"
defType TBlob     = "BLOB"

-- | Default compilation for a column attribute.
defColAttr :: ColAttr -> Text
defColAttr Primary       = "PRIMARY KEY"
defColAttr AutoIncrement = "AUTOINCREMENT"
defColAttr Required      = "NOT NULL"
defColAttr Optional      = "NULL"
defColAttr Unique        = "UNIQUE"
