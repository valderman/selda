{-# LANGUAGE GADTs, OverloadedStrings #-}
-- | SQL AST and pretty-printing.
module Database.Selda.SQL where
import Database.Selda.Column
import Database.Selda.SqlType
import Database.Selda.Types (TableName)
import Data.Monoid

-- | A source for an SQL query.
data SqlSource
 = TableName TableName
 | Product [SQL]
 | LeftJoin (Exp Bool) SQL SQL

-- | AST for SQL queries.
data SQL = SQL
  { cols      :: [SomeCol]
  , source    :: SqlSource
  , restricts :: [Exp Bool]
  , groups    :: [SomeCol]
  , ordering  :: [(Order, SomeCol)]
  , limits    :: Maybe (Int, Int)
  }

-- | The order in which to sort result rows.
data Order = Asc | Desc
  deriving (Show, Ord, Eq)

-- | A parameter to a prepared SQL statement.
data Param where
  Param :: Lit a -> Param

instance Show Param where
  show (Param l) = "Param " <> show l
