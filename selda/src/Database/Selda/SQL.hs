{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE RankNTypes, CPP, MultiParamTypeClasses #-}
-- | SQL AST and parameters for prepared statements.
module Database.Selda.SQL where
import Data.String
import Data.Text (Text)
import Database.Selda.Exp
import Database.Selda.SqlType
import Database.Selda.Types
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup (Semigroup (..))
#endif

instance Semigroup QueryFragment where
  (<>) = RawCat

data QueryFragment where
  RawText :: !Text -> QueryFragment
  RawExp  :: !(Exp SQL a) -> QueryFragment
  RawCat  :: !QueryFragment -> !QueryFragment -> QueryFragment

instance IsString QueryFragment where
  fromString = RawText . fromString

-- | A source for an SQL query.
data SqlSource
 = TableName !TableName
 | Product ![SQL]
 | Join !JoinType !(Exp SQL Bool) !SQL !SQL
 | Values ![SomeCol SQL] ![[Param]]
 | RawSql !QueryFragment
 | EmptyTable

-- | Type of join to perform.
data JoinType = InnerJoin | LeftJoin

-- | AST for SQL queries.
data SQL = SQL
  { cols      :: ![SomeCol SQL]
  , source    :: !SqlSource
  , restricts :: ![Exp SQL Bool]
  , groups    :: ![SomeCol SQL]
  , ordering  :: ![(Order, SomeCol SQL)]
  , limits    :: !(Maybe (Int, Int))
  , distinct  :: !Bool
  }

instance Names QueryFragment where
  allNamesIn (RawText _)  = []
  allNamesIn (RawExp e)   = allNamesIn e
  allNamesIn (RawCat a b) = allNamesIn a ++ allNamesIn b

instance Names SqlSource where
  allNamesIn (Product qs)   = concatMap allNamesIn qs
  allNamesIn (Join _ e l r) = allNamesIn e ++ concatMap allNamesIn [l, r]
  allNamesIn (Values vs _)  = allNamesIn vs
  allNamesIn (TableName _)  = []
  allNamesIn (RawSql r)     = allNamesIn r
  allNamesIn (EmptyTable)   = []

instance Names SQL where
  -- Note that we don't include @cols@ here: the names in @cols@ are not
  -- necessarily used, only declared.
  allNamesIn (SQL{..}) = concat
    [ allNamesIn groups
    , concatMap (allNamesIn . snd) ordering
    , allNamesIn restricts
    , allNamesIn source
    ]

-- | Build a plain SQL query with the given columns and source, with no filters,
--   ordering, etc.
sqlFrom :: [SomeCol SQL] -> SqlSource -> SQL
sqlFrom cs src = SQL
  { cols = cs
  , source = src
  , restricts = []
  , groups = []
  , ordering = []
  , limits = Nothing
  , distinct = False
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

-- | Create a parameter from the given value.
param :: SqlType a => a -> Param
param = Param . mkLit

-- | The SQL type of the given parameter.
paramType :: Param -> SqlTypeRep
paramType (Param p) = litType p
