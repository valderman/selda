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

    -- | Insert queries may have at most this many parameters; if an insertion
    --   has more parameters than this, it will be chunked.
    --
    --   Note that only insertions of multiple rows are chunked. If your table
    --   has more than this many columns, you should really rethink
    --   your database design.
  , ppMaxInsertParams :: Maybe Int
  }

-- | Default settings for pretty-printing.
--   Geared towards SQLite.
defPPConfig :: PPConfig
defPPConfig = PPConfig
    { ppType = defType
    , ppPlaceholder = T.cons '$' . T.pack . show
    , ppColAttrs = T.unwords . map defColAttr
    , ppAutoIncInsert = "NULL"
    , ppMaxInsertParams = Nothing
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
