{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables #-}
-- | Types representable in Selda's subset of SQL.
module Database.Selda.SqlType where
import Data.Text (Text)
import Data.Proxy

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
  show (LitJust x) = "Just " ++ show x
  show (LitNull)   = "Nothing"

instance SqlType Int where
  mkLit = LitI
  sqlType _ = "INTEGER"
  fromSql (SqlInt x) = x
  fromSql _          = error "fromSql: int column with non-int value"
instance SqlType Double where
  mkLit = LitD
  sqlType _ = "DOUBLE"
  fromSql (SqlFloat x) = x
  fromSql _            = error "fromSql: float column with non-float value"
instance SqlType Text where
  mkLit = LitS
  sqlType _ = "TEXT"
  fromSql (SqlString x) = x
  fromSql _             = error "fromSql: text column with non-text value"
instance SqlType Bool where
  mkLit = LitB
  sqlType _ = "INT"
  fromSql (SqlBool x) = x
  fromSql _           = error "fromSql: bool column with non-bool value"
instance SqlType a => SqlType (Maybe a) where
  mkLit (Just x) = LitJust $ mkLit x
  mkLit Nothing  = LitNull
  sqlType _ = sqlType (Proxy :: Proxy a)
  fromSql (SqlNull) = Nothing
  fromSql x         = Just $ fromSql x
