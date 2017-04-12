{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables #-}
-- | Types representable in Selda's subset of SQL.
module Database.Selda.SqlType where
import Data.Text (Text, pack, unpack)
import Data.Time
import Data.Proxy

-- | Format string used to represent date and time when
--   talking to the database backend.
sqlDateTimeFormat :: String
sqlDateTimeFormat = "%F %H:%M:%S%Q"

-- | Format string used to represent date when
--   talking to the database backend.
sqlDateFormat :: String
sqlDateFormat = "%F"

-- | Format string used to represent time of day when
--   talking to the database backend.
sqlTimeFormat :: String
sqlTimeFormat = "%H:%M:%S%Q"

-- | Any datatype representable in (Selda's subset of) SQL.
class SqlType a where
  mkLit :: a -> Lit a
  sqlType :: Proxy a -> Text
  fromSql :: SqlValue -> a

-- | An SQL mkLit.
data Lit a where
  LitS    :: !Text    -> Lit Text
  LitI    :: !Int     -> Lit Int
  LitD    :: !Double  -> Lit Double
  LitB    :: !Bool    -> Lit Bool
  LitTS   :: !Text    -> Lit UTCTime
  LitDate :: !Text    -> Lit Day
  LitTime :: !Text    -> Lit TimeOfDay
  LitJust :: !(Lit a) -> Lit (Maybe b)
  LitNull :: Lit (Maybe a)

-- | Some value that is representable in SQL.
data SqlValue where
  SqlInt    :: !Int    -> SqlValue
  SqlFloat  :: !Double -> SqlValue
  SqlString :: !Text   -> SqlValue
  SqlBool   :: !Bool   -> SqlValue
  SqlNull   :: SqlValue

instance Show SqlValue where
  show (SqlInt n)    = "SqlInt " ++ show n
  show (SqlFloat f)  = "SqlFloat " ++ show f
  show (SqlString s) = "SqlString " ++ show s
  show (SqlBool b)   = "SqlBool " ++ show b
  show (SqlNull)     = "SqlNull"

instance Show (Lit a) where
  show (LitS s)    = show s
  show (LitI i)    = show i
  show (LitD d)    = show d
  show (LitB b)    = show b
  show (LitTS s)   = show s
  show (LitDate s) = show s
  show (LitTime s) = show s
  show (LitJust x) = "Just " ++ show x
  show (LitNull)   = "Nothing"

instance SqlType Int where
  mkLit = LitI
  sqlType _ = "INTEGER"
  fromSql (SqlInt x) = x
  fromSql v          = error $ "fromSql: int column with non-int value: " ++ show v
instance SqlType Double where
  mkLit = LitD
  sqlType _ = "DOUBLE"
  fromSql (SqlFloat x) = x
  fromSql v            = error $ "fromSql: float column with non-float value: " ++ show v
instance SqlType Text where
  mkLit = LitS
  sqlType _ = "TEXT"
  fromSql (SqlString x) = x
  fromSql v             = error $ "fromSql: text column with non-text value: " ++ show v
instance SqlType Bool where
  mkLit = LitB
  sqlType _ = "INT"
  fromSql (SqlBool x) = x
  fromSql (SqlInt 0)  = False
  fromSql (SqlInt _)  = True
  fromSql v           = error $ "fromSql: bool column with non-bool value: " ++ show v
instance SqlType UTCTime where
  mkLit = LitTS . pack . formatTime defaultTimeLocale sqlDateTimeFormat
  sqlType _             = "DATETIME"
  fromSql (SqlString s) =
    case parseTimeM True defaultTimeLocale sqlDateTimeFormat (unpack s) of
      Just t -> t
      _      -> error $ "fromSql: bad datetime string: " ++ unpack s
  fromSql v             = error $ "fromSql: datetime column with non-datetime value: " ++ show v
instance SqlType Day where
  mkLit = LitDate . pack . formatTime defaultTimeLocale sqlDateFormat
  sqlType _             = "DATE"
  fromSql (SqlString s) =
    case parseTimeM True defaultTimeLocale sqlDateFormat (unpack s) of
      Just t -> t
      _      -> error $ "fromSql: bad date string: " ++ unpack s
  fromSql v             = error $ "fromSql: date column with non-date value: " ++ show v
instance SqlType TimeOfDay where
  mkLit = LitTime . pack . formatTime defaultTimeLocale sqlTimeFormat
  sqlType _             = "TIME"
  fromSql (SqlString s) =
    case parseTimeM True defaultTimeLocale sqlTimeFormat (unpack s) of
      Just t -> t
      _      -> error $ "fromSql: bad time string: " ++ unpack s
  fromSql v             = error $ "fromSql: time column with non-time value: " ++ show v
instance SqlType a => SqlType (Maybe a) where
  mkLit (Just x) = LitJust $ mkLit x
  mkLit Nothing  = LitNull
  sqlType _ = sqlType (Proxy :: Proxy a)
  fromSql (SqlNull) = Nothing
  fromSql x         = Just $ fromSql x
