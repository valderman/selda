{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, DefaultSignatures, DeriveGeneric #-}
-- | Types representable as columns in Selda's subset of SQL.
module Database.Selda.SqlType
  ( SqlType (..), SqlEnum (..)
  , Lit (..), UUID, UUID', RowID, ID, SqlValue (..), SqlTypeRep (..)
  , invalidRowId, isInvalidRowId, toRowId, fromRowId
  , fromId, toId, invalidId, isInvalidId, untyped
  , compLit, litType
  , sqlDateTimeFormat, sqlDateFormat, sqlTimeFormat
  , typedUuid, untypedUuid
  ) where
import Control.Applicative ((<|>))
import Control.Exception (Exception (..), throw)
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int32, Int64)
import Data.Maybe (fromJust)
import Data.Proxy ( Proxy(..) )
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as LazyText
import Data.Time
    ( defaultTimeLocale,
      parseTimeM,
      Day(ModifiedJulianDay),
      UTCTime(UTCTime),
      ParseTime,
      TimeOfDay(TimeOfDay) )
import Data.Typeable ( Typeable )
import Data.UUID.Types (UUID, toString, fromByteString, nil)
import GHC.Generics (Generic)

-- | Format string used to represent date and time when
--   representing timestamps as text.
--   If at all possible, use 'SqlUTCTime' instead.
sqlDateTimeFormat :: String
sqlDateTimeFormat = "%F %H:%M:%S%Q%z"

-- | Format string used to represent date when
--   representing dates as text.
--   If at all possible, use 'SqlDate' instead.
sqlDateFormat :: String
sqlDateFormat = "%F"

-- | Format string used to represent time of day when
--   representing time as text.
--   If at all possible, use 'SqlTime' instead.
sqlTimeFormat :: String
sqlTimeFormat = "%H:%M:%S%Q%z"

-- | Representation of an SQL type.
data SqlTypeRep
  = TText
  | TRowID
  | TInt64
  | TInt32
  | TFloat
  | TBool
  | TDateTime
  | TDate
  | TTime
  | TBlob
  | TUUID
  | TJSON
    deriving (Show, Eq, Ord)

-- | Any datatype representable in (Selda's subset of) SQL.
class Typeable a => SqlType a where
  -- | Create a literal of this type.
  mkLit :: a -> Lit a
  default mkLit :: (Typeable a, SqlEnum a) => a -> Lit a
  mkLit = LCustom TText . LText . toText

  -- | The SQL representation for this type.
  sqlType :: Proxy a -> SqlTypeRep
  sqlType _ = litType (defaultValue :: Lit a)

  -- | Convert an SqlValue into this type.
  fromSql :: SqlValue -> a
  default fromSql :: (Typeable a, SqlEnum a) => SqlValue -> a
  fromSql = fromText . fromSql

  -- | Default value when using 'def' at this type.
  defaultValue :: Lit a
  default defaultValue :: (Typeable a, SqlEnum a) => Lit a
  defaultValue = mkLit (minBound :: a)

-- | Any type that's bounded, enumerable and has a text representation, and
--   thus representable as a Selda enumerable.
--
--   While it would be more efficient to store enumerables as integers, this
--   makes hand-rolled SQL touching the values inscrutable, and will break if
--   the user a) derives Enum and b) changes the order of their constructors.
--   Long-term, this should be implemented in PostgreSQL as a proper enum
--   anyway, which mostly renders the performance argument moot.
class (Typeable a, Bounded a, Enum a) => SqlEnum a where
  toText :: a -> Text
  fromText :: Text -> a

instance {-# OVERLAPPABLE #-}
    (Typeable a, Bounded a, Enum a, Show a, Read a) => SqlEnum a where
  toText = pack . show
  fromText = read . unpack

-- | An SQL literal.
data Lit a where
  LText     :: !Text       -> Lit Text
  LInt32    :: !Int32      -> Lit Int32
  LInt64    :: !Int64      -> Lit Int64
  LDouble   :: !Double     -> Lit Double
  LBool     :: !Bool       -> Lit Bool
  LDateTime :: !UTCTime    -> Lit UTCTime
  LDate     :: !Day        -> Lit Day
  LTime     :: !TimeOfDay  -> Lit TimeOfDay
  LJust     :: SqlType a => !(Lit a) -> Lit (Maybe a)
  LBlob     :: !ByteString -> Lit ByteString
  LNull     :: SqlType a => Lit (Maybe a)
  LCustom   :: SqlTypeRep  -> Lit a -> Lit b
  LUUID     :: !UUID       -> Lit UUID

-- | The SQL type representation for the given literal.
litType :: Lit a -> SqlTypeRep
litType (LText{})     = TText
litType (LInt32{})    = TInt32
litType (LInt64{})    = TInt64
litType (LDouble{})   = TFloat
litType (LBool{})     = TBool
litType (LDateTime{}) = TDateTime
litType (LDate{})     = TDate
litType (LTime{})     = TTime
litType (LJust x)     = litType x
litType (LBlob{})     = TBlob
litType (x@LNull)     = sqlType (proxyFor x)
  where
    proxyFor :: Lit (Maybe a) -> Proxy a
    proxyFor _ = Proxy
litType (LCustom t _) = t
litType (LUUID{})     = TUUID

instance Eq (Lit a) where
  a == b = compLit a b == EQ

instance Ord (Lit a) where
  compare = compLit

-- | Constructor tag for all literals. Used for Ord instance.
litConTag :: Lit a -> Int
litConTag (LText{})     = 0
litConTag (LInt32{})    = 2
litConTag (LInt64{})    = 3
litConTag (LDouble{})   = 4
litConTag (LBool{})     = 5
litConTag (LDateTime{}) = 6
litConTag (LDate{})     = 7
litConTag (LTime{})     = 8
litConTag (LJust{})     = 9
litConTag (LBlob{})     = 10
litConTag (LNull)       = 11
litConTag (LCustom{})   = 12
litConTag (LUUID{})     = 13

-- | Compare two literals of different type for equality.
compLit :: Lit a -> Lit b -> Ordering
compLit (LText x)     (LText x')     = x `compare` x'
compLit (LInt32 x)    (LInt32 x')    = x `compare` x'
compLit (LInt64 x)    (LInt64 x')    = x `compare` x'
compLit (LDouble x)   (LDouble x')   = x `compare` x'
compLit (LBool x)     (LBool x')     = x `compare` x'
compLit (LDateTime x) (LDateTime x') = x `compare` x'
compLit (LDate x)     (LDate x')     = x `compare` x'
compLit (LTime x)     (LTime x')     = x `compare` x'
compLit (LBlob x)     (LBlob x')     = x `compare` x'
compLit (LJust x)     (LJust x')     = x `compLit` x'
compLit (LCustom _ x) (LCustom _ x') = x `compLit` x'
compLit (LUUID x)     (LUUID x')     = x `compare` x'
compLit a             b              = litConTag a `compare` litConTag b

-- | Some value that is representable in SQL.
data SqlValue where
  SqlInt32   :: !Int32      -> SqlValue
  SqlInt64   :: !Int64      -> SqlValue
  SqlFloat   :: !Double     -> SqlValue
  SqlString  :: !Text       -> SqlValue
  SqlBool    :: !Bool       -> SqlValue
  SqlBlob    :: !ByteString -> SqlValue
  SqlUTCTime :: !UTCTime    -> SqlValue
  SqlTime    :: !TimeOfDay  -> SqlValue
  SqlDate    :: !Day        -> SqlValue
  SqlNull    :: SqlValue

instance Show SqlValue where
  show (SqlInt32 n)   = "SqlInt32 " ++ show n
  show (SqlInt64 n)   = "SqlInt64 " ++ show n
  show (SqlFloat f)   = "SqlFloat " ++ show f
  show (SqlString s)  = "SqlString " ++ show s
  show (SqlBool b)    = "SqlBool " ++ show b
  show (SqlBlob b)    = "SqlBlob " ++ show b
  show (SqlUTCTime t) = "SqlUTCTime " ++ show t
  show (SqlTime t)    = "SqlTime " ++ show t
  show (SqlDate d)    = "SqlDate " ++ show d
  show (SqlNull)      = "SqlNull"

instance Show (Lit a) where
  show (LText s)     = show s
  show (LInt32 i)    = show i
  show (LInt64 i)    = show i
  show (LDouble d)   = show d
  show (LBool b)     = show b
  show (LDateTime s) = show s
  show (LDate s)     = show s
  show (LTime s)     = show s
  show (LBlob b)     = show b
  show (LJust x)     = "Just " ++ show x
  show (LNull)       = "Nothing"
  show (LCustom _ l) = show l
  show (LUUID u)     = toString u

-- | A row identifier for some table.
--   This is the type of auto-incrementing primary keys.
newtype RowID = RowID Int64
  deriving (Eq, Ord, Typeable, Generic)
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
--   Use with caution, preferably only when reading user input.
toRowId :: Int64 -> RowID
toRowId = RowID

-- | Inspect a row identifier.
fromRowId :: RowID -> Int64
fromRowId (RowID n) = n

-- | A typed row identifier.
--   Generic tables should use this instead of 'RowID'.
--   Use 'untyped' to erase the type of a row identifier, and @cast@ from the
--   "Database.Selda.Unsafe" module if you for some reason need to add a type
--   to a row identifier.
newtype ID a = ID {untyped :: RowID}
  deriving (Eq, Ord, Typeable, Generic)
instance Show (ID a) where
  show = show . untyped

-- | An UUID identifying a database row.
newtype UUID' a = UUID { untypedUuid :: UUID }
  deriving (Eq, Ord, Typeable, Generic)
instance Show (UUID' a) where
  show = show . untypedUuid

-- | Convert an untyped UUID to a typed one.
--   Use sparingly, preferably only during deserialization.
typedUuid :: UUID -> UUID' a
typedUuid = UUID

-- | Create a typed row identifier from an integer.
--   Use with caution, preferably only when reading user input.
toId :: Int64 -> ID a
toId = ID . toRowId

-- | Create a typed row identifier from an integer.
--   Use with caution, preferably only when reading user input.
fromId :: ID a -> Int64
fromId (ID i) = fromRowId i

-- | A typed row identifier which is guaranteed to not match any row in any
--   table.
invalidId :: ID a
invalidId = ID invalidRowId

-- | Is the given typed row identifier invalid? I.e. is it guaranteed to not
--   match any row in any table?
isInvalidId :: ID a -> Bool
isInvalidId = isInvalidRowId . untyped

fromSqlError :: String -> a
fromSqlError = throw . FromSqlError

newtype FromSqlError = FromSqlError String
instance Show FromSqlError where
  show (FromSqlError e) = "[SELDA BUG] fromSql: " ++ e
instance Exception FromSqlError

instance SqlType RowID where
  mkLit (RowID n) = LCustom TRowID (LInt64 n)
  sqlType _ = TRowID
  fromSql (SqlInt64 x) = RowID x
  fromSql v          = fromSqlError $ "RowID column with non-int value: " ++ show v
  defaultValue = mkLit invalidRowId

instance Typeable a => SqlType (ID a) where
  mkLit (ID n) = LCustom TRowID (mkLit n)
  sqlType _ = TRowID
  fromSql = ID . fromSql
  defaultValue = mkLit (ID invalidRowId)

instance SqlType Int where
  mkLit n = LCustom TInt64 (LInt64 $ fromIntegral n)
  sqlType _ = TInt64
  fromSql (SqlInt64 x) = fromIntegral x
  fromSql v            = fromSqlError $ "int column with non-int value: " ++ show v
  defaultValue = mkLit (0 :: Int)

instance SqlType Int64 where
  mkLit = LInt64
  sqlType _ = TInt64
  fromSql (SqlInt64 x) = x
  fromSql v          = fromSqlError $ "int64 column with non-int value: " ++ show v
  defaultValue = LInt64 0

instance SqlType Int32 where
  mkLit = LInt32
  sqlType _ = TInt32
  fromSql (SqlInt32 x) = x
  fromSql v          = fromSqlError $ "int32 column with non-int value: " ++ show v
  defaultValue = LInt32 0

instance SqlType Double where
  mkLit = LDouble
  sqlType _ = TFloat
  fromSql (SqlFloat x) = x
  fromSql v            = fromSqlError $ "float column with non-float value: " ++ show v
  defaultValue = LDouble 0

instance SqlType Text where
  mkLit = LText
  sqlType _ = TText
  fromSql (SqlString x) = x
  fromSql v             = fromSqlError $ "text column with non-text value: " ++ show v
  defaultValue = LText ""

instance SqlType LazyText.Text where
  mkLit = LCustom TText . LText . mconcat . LazyText.toChunks
  sqlType _ = TText
  fromSql (SqlString x) = LazyText.fromChunks [x]
  fromSql v             = fromSqlError $ "lazy text column with non-text value: " ++ show v
  defaultValue = mkLit ""

instance SqlType Bool where
  mkLit = LBool
  sqlType _ = TBool
  fromSql (SqlBool x)  = x
  fromSql (SqlInt32 0) = False
  fromSql (SqlInt32 _) = True
  fromSql (SqlInt64 0) = False
  fromSql (SqlInt64 _) = True
  fromSql v            = fromSqlError $ "bool column with non-bool value: " ++ show v
  defaultValue = LBool False

instance SqlType UTCTime where
  mkLit = LDateTime
  sqlType _ = TDateTime
  fromSql (SqlUTCTime t) = t
  fromSql (SqlString s) =
    case withWeirdTimeZone sqlDateTimeFormat (unpack s) of
      Just t -> t
      _      -> fromSqlError $ "bad datetime string: " ++ unpack s
  fromSql v = fromSqlError $ "datetime column with non-datetime value: " ++ show v
  defaultValue = LDateTime $ UTCTime (ModifiedJulianDay 40587) 0

instance SqlType Day where
  mkLit = LDate
  sqlType _ = TDate
  fromSql (SqlDate d) = d
  fromSql (SqlString s) =
    case parseTimeM True defaultTimeLocale sqlDateFormat (unpack s) of
      Just t -> t
      _      -> fromSqlError $ "bad date string: " ++ unpack s
  fromSql v = fromSqlError $ "date column with non-date value: " ++ show v
  defaultValue = LDate $ ModifiedJulianDay 40587

instance SqlType TimeOfDay where
  mkLit = LTime
  sqlType _ = TTime
  fromSql (SqlTime s) = s
  fromSql (SqlString s) =
    case withWeirdTimeZone sqlTimeFormat (unpack s) of
      Just t -> t
      _      -> fromSqlError $ "bad time string: " ++ unpack s
  fromSql v = fromSqlError $ "time column with non-time value: " ++ show v
  defaultValue = LTime $ TimeOfDay 0 0 0

-- | Both PostgreSQL and SQLite to weird things with time zones.
--   Long term solution is to use proper binary types internally for
--   time values, so this is really just an interim solution.
withWeirdTimeZone :: ParseTime t => String -> String -> Maybe t
withWeirdTimeZone fmt s =
  parseTimeM True defaultTimeLocale fmt (s++"00")
  <|> parseTimeM True defaultTimeLocale fmt s
  <|> parseTimeM True defaultTimeLocale fmt (s++"+0000")

instance SqlType ByteString where
  mkLit = LBlob
  sqlType _ = TBlob
  fromSql (SqlBlob x) = x
  fromSql v           = fromSqlError $ "blob column with non-blob value: " ++ show v
  defaultValue = LBlob empty

instance SqlType BSL.ByteString where
  mkLit = LCustom TBlob . LBlob . BSL.toStrict
  sqlType _ = TBlob
  fromSql (SqlBlob x) = BSL.fromStrict x
  fromSql v           = fromSqlError $ "blob column with non-blob value: " ++ show v
  defaultValue = LCustom TBlob (LBlob empty)

-- | @defaultValue@ for UUIDs is the all-zero RFC4122 nil UUID.
instance SqlType UUID where
  mkLit = LUUID
  sqlType _ = TUUID
  fromSql (SqlBlob x) = fromJust . fromByteString $ BSL.fromStrict x
  fromSql v           = fromSqlError $ "UUID column with non-blob value: " ++ show v
  defaultValue = LUUID nil

-- | @defaultValue@ for UUIDs is the all-zero RFC4122 nil UUID.
instance Typeable a => SqlType (UUID' a) where
  mkLit = LCustom TUUID . LUUID . untypedUuid
  sqlType _ = TUUID
  fromSql = typedUuid . fromSql
  defaultValue = LCustom TUUID (LUUID nil)

instance SqlType a => SqlType (Maybe a) where
  mkLit (Just x) = LJust $ mkLit x
  mkLit Nothing  = LNull
  sqlType _ = sqlType (Proxy :: Proxy a)
  fromSql (SqlNull) = Nothing
  fromSql x         = Just $ fromSql x
  defaultValue = LNull

instance SqlType Ordering
