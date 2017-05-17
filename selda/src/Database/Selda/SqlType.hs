{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}
-- | Types representable in Selda's subset of SQL.
module Database.Selda.SqlType
  ( Lit (..), RowID, SqlValue (..), SqlType
  , invalidRowId, isInvalidRowId, unsafeRowId
  , mkLit, sqlType, fromSql, defaultValue
  , compLit
  , sqlDateTimeFormat, sqlDateFormat, sqlTimeFormat
  ) where
import Data.Proxy
import Data.Text (Text, pack, unpack)
import Data.Time
import Data.Typeable

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
class Typeable a => SqlType a where
  mkLit        :: a -> Lit a
  sqlType      :: Proxy a -> Text
  fromSql      :: SqlValue -> a
  defaultValue :: Lit a

-- | An SQL literal.
data Lit a where
  LText     :: !Text    -> Lit Text
  LInt      :: !Int     -> Lit Int
  LDouble   :: !Double  -> Lit Double
  LBool     :: !Bool    -> Lit Bool
  LDateTime :: !Text    -> Lit UTCTime
  LDate     :: !Text    -> Lit Day
  LTime     :: !Text    -> Lit TimeOfDay
  LJust     :: !(Lit a) -> Lit (Maybe a)
  LNull     :: Lit (Maybe a)
  LCustom   :: !(Lit a) -> Lit b

instance Eq (Lit a) where
  a == b = compLit a b == EQ

instance Ord (Lit a) where
  compare = compLit

-- | Constructor tag for all literals. Used for Ord instance.
litConTag :: Lit a -> Int
litConTag (LText{})     = 0
litConTag (LInt{})      = 1
litConTag (LDouble{})   = 2
litConTag (LBool{})     = 3
litConTag (LDateTime{}) = 4
litConTag (LDate{})     = 5
litConTag (LTime{})     = 6
litConTag (LJust{})     = 7
litConTag (LNull)       = 8

-- | Compare two literals of different type for equality.
compLit :: Lit a -> Lit b -> Ordering
compLit (LText x)     (LText x')     = x `compare` x'
compLit (LInt x)      (LInt x')      = x `compare` x'
compLit (LDouble x)   (LDouble x')   = x `compare` x'
compLit (LBool x)     (LBool x')     = x `compare` x'
compLit (LDateTime x) (LDateTime x') = x `compare` x'
compLit (LDate x)     (LDate x')     = x `compare` x'
compLit (LTime x)     (LTime x')     = x `compare` x'
compLit (LJust x)     (LJust x')     = x `compLit` x'
compLit a             b              = litConTag a `compare` litConTag b

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
  show (LText s)     = show s
  show (LInt i)      = show i
  show (LDouble d)   = show d
  show (LBool b)     = show b
  show (LDateTime s) = show s
  show (LDate s)     = show s
  show (LTime s)     = show s
  show (LJust x)     = "Just " ++ show x
  show (LNull)       = "Nothing"

-- | A row identifier for some table.
--   This is the type of auto-incrementing primary keys.
newtype RowID = RowID Int
  deriving (Eq, Typeable)
instance Show RowID where
  show (RowID n) = show n

-- | A row identifier which is guaranteed to not match any row in any table.
invalidRowId :: RowID
invalidRowId = RowID (-1)

-- | Is the given row identifier invalid? I.e. is it guaranteed to not match any
--   row in any table?
isInvalidRowId :: RowID -> Bool
isInvalidRowId (RowID n) = n < 0

-- | Create a row identifier from an integer.
--   A row identifier created using this function is not guaranteed to be a
--   valid row identifier.
--   Do not use unless you are absolutely sure what you're doing.
unsafeRowId :: Int -> RowID
unsafeRowId = RowID

instance SqlType RowID where
  mkLit (RowID n) = LCustom $ LInt n
  sqlType _ = sqlType (Proxy :: Proxy Int)
  fromSql (SqlInt x) = RowID x
  fromSql v          = error $ "fromSql: RowID column with non-int value: " ++ show v
  defaultValue = mkLit invalidRowId

instance SqlType Int where
  mkLit = LInt
  sqlType _ = "INTEGER"
  fromSql (SqlInt x) = x
  fromSql v          = error $ "fromSql: int column with non-int value: " ++ show v
  defaultValue = LInt 0

instance SqlType Double where
  mkLit = LDouble
  sqlType _ = "DOUBLE"
  fromSql (SqlFloat x) = x
  fromSql v            = error $ "fromSql: float column with non-float value: " ++ show v
  defaultValue = LDouble 0

instance SqlType Text where
  mkLit = LText
  sqlType _ = "TEXT"
  fromSql (SqlString x) = x
  fromSql v             = error $ "fromSql: text column with non-text value: " ++ show v
  defaultValue = LText ""

instance SqlType Bool where
  mkLit = LBool
  sqlType _ = "INT"
  fromSql (SqlBool x) = x
  fromSql (SqlInt 0)  = False
  fromSql (SqlInt _)  = True
  fromSql v           = error $ "fromSql: bool column with non-bool value: " ++ show v
  defaultValue = LBool False

instance SqlType UTCTime where
  mkLit = LDateTime . pack . formatTime defaultTimeLocale sqlDateTimeFormat
  sqlType _             = "DATETIME"
  fromSql (SqlString s) =
    case parseTimeM True defaultTimeLocale sqlDateTimeFormat (unpack s) of
      Just t -> t
      _      -> error $ "fromSql: bad datetime string: " ++ unpack s
  fromSql v             = error $ "fromSql: datetime column with non-datetime value: " ++ show v
  defaultValue = LDateTime "1970-01-01 00:00:00"

instance SqlType Day where
  mkLit = LDate . pack . formatTime defaultTimeLocale sqlDateFormat
  sqlType _             = "DATE"
  fromSql (SqlString s) =
    case parseTimeM True defaultTimeLocale sqlDateFormat (unpack s) of
      Just t -> t
      _      -> error $ "fromSql: bad date string: " ++ unpack s
  fromSql v             = error $ "fromSql: date column with non-date value: " ++ show v
  defaultValue = LDate "1970-01-01"

instance SqlType TimeOfDay where
  mkLit = LTime . pack . formatTime defaultTimeLocale sqlTimeFormat
  sqlType _             = "TIME"
  fromSql (SqlString s) =
    case parseTimeM True defaultTimeLocale sqlTimeFormat (unpack s) of
      Just t -> t
      _      -> error $ "fromSql: bad time string: " ++ unpack s
  fromSql v             = error $ "fromSql: time column with non-time value: " ++ show v
  defaultValue = LTime "00:00:00"

instance SqlType a => SqlType (Maybe a) where
  mkLit (Just x) = LJust $ mkLit x
  mkLit Nothing  = LNull
  sqlType _ = sqlType (Proxy :: Proxy a)
  fromSql (SqlNull) = Nothing
  fromSql x         = Just $ fromSql x
  defaultValue = LNull
