{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Selda.Query.Type where
import Control.Monad.State
import Database.Selda.SQL
import Database.Selda.Column

-- | An SQL query.
newtype Query s a = Query {unQ :: State GenState a}
  deriving (Functor, Applicative, Monad)

-- | Run a query computation from an initial state.
runQueryM :: Int -> Query s a -> (a, GenState)
runQueryM n = flip runState (initState {nameSupply = n}) . unQ

-- | SQL generation internal state.
--   Contains the subqueries and static (i.e. not dependent on any subqueries)
--   restrictions of the query currently being built, as well as a name supply
--   for column renaming.
data GenState = GenState
  { sources         :: [SQL]
  , staticRestricts :: [Exp Bool]
  , groupCols       :: [SomeCol]
  , nameSupply      :: Int
  }

-- | Initial state: no subqueries, no restrictions.
initState :: GenState
initState = GenState
  { sources = []
  , staticRestricts = []
  , groupCols = []
  , nameSupply = 0
  }
