{-# LANGUAGE GADTs, OverloadedStrings #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, UndecidableInstances #-}
-- | SQL AST and parameters for prepared statements.
module Database.Selda.SQL where
import Database.Selda.Column
import Database.Selda.SqlType
import Database.Selda.Types
import Control.Exception
import Data.Monoid
import System.IO.Unsafe

-- | A source for an SQL query.
data SqlSource
 = TableName !TableName
 | Product ![SQL]
 | LeftJoin !(Exp Bool) !SQL !SQL
 | Values ![SomeCol] ![[Param]]
 | EmptyTable

-- | AST for SQL queries.
data SQL = SQL
  { cols      :: ![SomeCol]
  , source    :: !SqlSource
  , restricts :: ![Exp Bool]
  , groups    :: ![SomeCol]
  , ordering  :: ![(Order, SomeCol)]
  , limits    :: !(Maybe (Int, Int))
  }

-- | The order in which to sort result rows.
data Order = Asc | Desc
  deriving (Show, Ord, Eq)

-- | A parameter to a prepared SQL statement.
data Param where
  Param :: !(Lit a) -> Param

instance Show Param where
  show (Param l) = "Param " <> show l

instance Eq Param where
  Param a == Param b = compLit a b == EQ
instance Ord Param where
  compare (Param a) (Param b) = compLit a b

-- | Exception indicating the use of a default value.
--   If any values throwing this during evaluation of @param xs@ will be
--   replaced by their default value.
data DefaultValueException = DefaultValueException
  deriving Show
instance Exception DefaultValueException

-- | An inductive tuple of Haskell-level values (i.e. @Int :*: Maybe Text@)
--   which can be inserted into a table.
class Insert a where
  params :: a -> [Maybe Param]
instance (SqlType a, Insert b) => Insert (a :*: b) where
  params (a :*: b) = unsafePerformIO $ do
    res <- try $ return $! a
    case res of
      Right a'                   -> return $ Just (Param (mkLit a')) : params b
      Left DefaultValueException -> return $ Nothing : params b
instance {-# OVERLAPPABLE #-} SqlType a => Insert a where
  params a = unsafePerformIO $ do
    res <- try $ return $! a
    case res of
      Right a'                   -> return [Just $ Param (mkLit a')]
      Left DefaultValueException -> return [Nothing]
