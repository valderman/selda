{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Selda.Query.Type where
import Control.Monad.State
import Database.Selda.SQL
import Database.Selda.Column

-- | An SQL query.
newtype Query s a = Query {unQ :: State GenState a}
  deriving (Functor, Applicative, Monad)

-- | Run a query computation from an initial state.
runQueryM :: Query s a -> (a, GenState)
runQueryM = flip runState initState . unQ

-- | Run a query computation in isolation, but reusing the current name supply.
isolate :: Query s a -> State GenState (GenState, a)
isolate (Query q) = do
  st <- get
  put $ initState {nameSupply = nameSupply st}
  x <- q
  st' <- get
  put $ st {nameSupply = nameSupply st'}
  return (st', x)

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
