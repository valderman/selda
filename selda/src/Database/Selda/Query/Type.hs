{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, CPP #-}
module Database.Selda.Query.Type where
import Control.Monad.State.Strict
#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid
#endif
import Data.IORef
import Data.Text (pack)
import Database.Selda.SQL
import Database.Selda.Column
import Database.Selda.Types (ColName, mkColName, addColSuffix)
import System.IO.Unsafe

type Scope = Int
type Ident = Int

-- | Try using a global name supply go fix issue #28.
{-# NOINLINE globalNameSupply #-}
globalNameSupply :: IORef Int
globalNameSupply = unsafePerformIO $ newIORef 0

-- | A name, consisting of a scope and an identifier.
data Name = Name Scope Ident

instance Show Name where
  show (Name 0 n) = concat [show n]
  show (Name s n) = concat [show s, "s_", show n]

-- | An SQL query.
newtype Query s a = Query {unQ :: State GenState a}
  deriving (Functor, Applicative, Monad)

-- | Run a query computation from an initial state.
runQueryM :: Scope -> Query s a -> (a, GenState)
runQueryM scope = flip runState (initState scope) . unQ

-- | Run a query computation in isolation, but reusing the current name supply.
isolate :: Query s a -> State GenState (GenState, a)
isolate (Query q) = do
  st <- get
  put $ initState (nameScope st)
  x <- q
  st' <- get
  put st
  return (st', x)

-- | SQL generation internal state.
--   Contains the subqueries and static (i.e. not dependent on any subqueries)
--   restrictions of the query currently being built, as well as a name supply
--   for column renaming.
data GenState = GenState
  { sources         :: ![SQL]
  , staticRestricts :: ![Exp SQL Bool]
  , groupCols       :: ![SomeCol SQL]
  , nameScope       :: !Int
  }

-- | Initial state: no subqueries, no restrictions.
initState :: Int -> GenState
initState scope = GenState
  { sources = []
  , staticRestricts = []
  , groupCols = []
  , nameScope  = scope
  }

renameAll :: [UntypedCol sql] -> State GenState [SomeCol sql]
renameAll = fmap concat . mapM rename

-- | Generate a unique name for the given column.
--   Flattens any composite columns.
rename :: UntypedCol sql -> State GenState [SomeCol sql]
rename (Untyped col) = do
    n <- freshId
    return [Named (newName n) col]
  where
    newName ns =
      case col of
        Col n -> addColSuffix n $ "_" <> pack (show ns)
        _     -> mkColName $ "tmp_" <> pack (show ns)

-- | Get a guaranteed unique identifier.
freshId :: State GenState Name
freshId = do
  st <- get
  name <- modifyNS $ \name -> (succ name, name)
  return (Name (nameScope st) name)

{-# NOINLINE modifyNS #-}
modifyNS :: (Int -> (Int, Int)) -> State a Int
modifyNS f = do
  return $! unsafePerformIO (atomicModifyIORef' globalNameSupply f)

-- | Get a guaranteed unique column name.
freshName :: State GenState ColName
freshName = do
  n <- freshId
  return $ mkColName $ "tmp_" <> pack (show n)
