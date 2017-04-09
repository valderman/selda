{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
-- | API for executing queries and building backends.
module Database.Selda.Backend
  ( -- * High-level API
    Result, Res, MonadIO (..), SeldaT
  , query
  , insert, insert_, insertWithPK
  , update, update_
  , deleteFrom, deleteFrom_
  , createTable, tryCreateTable
  , dropTable, tryDropTable
    -- * Low-level API for creating backends and custom queries.
  , MonadTrans (..), MonadThrow (..), MonadCatch (..)
  , QueryRunner, Param (..), Lit (..), Proxy (..), SqlValue (..)
  , SeldaBackend (..)
  , exec, queryWith, runSeldaT
  ) where
import Database.Selda.Column
import Database.Selda.Compile
import Database.Selda.Query.Type
import Database.Selda.SQL (Param (..))
import Database.Selda.SqlType
import Database.Selda.Table
import Database.Selda.Table.Compile
import Data.Proxy
import Data.Text (Text)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader

-- | A function which executes a query and gives back a list of extensible
--   tuples; one tuple per result row, and one tuple element per column.
type QueryRunner a = Text -> [Param] -> IO a

-- | A collection of functions making up a Selda backend.
data SeldaBackend = SeldaBackend
  { -- | Execute an SQL statement.
    runStmt       :: QueryRunner (Int, [[SqlValue]])

    -- | Execute an SQL statement and return the last inserted primary key,
    --   where the primary key is auto-incrementing.
    --   Backends must take special care to make this thread-safe.
  , runStmtWithPK :: QueryRunner Int
  }

-- | Monad transformer adding Selda SQL capabilities.
newtype SeldaT m a = S {unS :: ReaderT SeldaBackend m a}
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask, MonadTrans
           )

-- | Run a Selda transformer. Backends should use this to implement their
--   @withX@ functions.
runSeldaT :: SeldaT m a -> SeldaBackend -> m a
runSeldaT m = runReaderT (unS m)

-- | Run a query within a Selda transformer.
--   Selda transformers are entered using backend-specific @withX@ functions,
--   such as 'withSQLite' from the SQLite backend.
query :: forall s m a. (MonadIO m, Result a) => Query s a -> SeldaT m [Res a]
query q = S $ do
  backend <- ask
  queryWith (runStmt backend) q

-- | Insert the given values into the given table. All columns of the table
--   must be present, EXCEPT any auto-incrementing primary keys ('autoPrimary'
--   columns), which are always assigned their default value.
--   Returns the number of rows that were inserted.
--
--   To insert a list of tuples into a table with auto-incrementing primary key:
--
-- > people :: Table (Auto Int :*: Text :*: Int :*: Maybe Text)
-- > people = table "ppl"
-- >        $ autoPrimary "id"
-- >        ¤ required "name"
-- >        ¤ required "age"
-- >        ¤ optional "pet"
-- >
-- > main = withSQLite "my_database.sqlite" $ do
-- >   insert_ people
-- >     [ "Link"  :*: 125 :*: Just "horse"
-- >     , "Zelda" :*: 119 :*: Nothing
-- >     , ...
-- >     ]
--
--   Again, note that ALL non-auto-incrementing fields must be present in the
--   tuples to be inserted, including primary keys without the auto-increment
--   attribute.
insert :: (MonadIO m, Insert (InsertCols a))
       => Table a -> [InsertCols a] -> SeldaT m Int
insert _ [] = return 0
insert t cs = uncurry exec $ compileInsert t cs

-- | Like 'insert', but does not return anything.
--   Use this when you really don't care about how many rows were inserted.
insert_ :: (MonadIO m, Insert (InsertCols a))
        => Table a -> [InsertCols a] -> SeldaT m ()
insert_ t cs = void $ insert t cs

-- | Like 'insert', but returns the primary key of the last inserted row.
--   Attempting 
insertWithPK :: (MonadIO m, HasAutoPrimary a, Insert (InsertCols a))
                => Table a -> [InsertCols a] -> SeldaT m Int
insertWithPK t cs = S $ do
  backend <- ask
  liftIO . uncurry (runStmtWithPK backend) $ compileInsert t cs

-- | Update the given table using the given update function, for all rows
--   matching the given predicate. Returns the number of updated rows.
update :: (MonadIO m, Columns (Cols s a), Result (Cols s a))
       => Table a                  -- ^ The table to update.
       -> (Cols s a -> Col s Bool) -- ^ Predicate.
       -> (Cols s a -> Cols s a)   -- ^ Update function.
       -> SeldaT m Int
update tbl check upd = uncurry exec $ compileUpdate tbl upd check

-- | Like 'update', but doesn't return the number of updated rows.
update_ :: (MonadIO m, Columns (Cols s a), Result (Cols s a))
       => Table a
       -> (Cols s a -> Col s Bool)
       -> (Cols s a -> Cols s a)
       -> SeldaT m ()
update_ tbl check upd = void $ update tbl check upd

-- | From the given table, delete all rows matching the given predicate.
--   Returns the number of deleted rows.
deleteFrom :: (MonadIO m, Columns (Cols s a))
           => Table a -> (Cols s a -> Col s Bool) -> SeldaT m Int
deleteFrom tbl f = uncurry exec $ compileDelete tbl f

-- | Like 'deleteFrom', but does not return the number of deleted rows.
deleteFrom_ :: (MonadIO m, Columns (Cols s a))
            => Table a -> (Cols s a -> Col s Bool) -> SeldaT m ()
deleteFrom_ tbl f = void . uncurry exec $ compileDelete tbl f

-- | Create a table from the given schema.
createTable :: MonadIO m => Table a -> SeldaT m ()
createTable = void . flip exec [] . compileCreateTable Fail

-- | Create a table from the given schema, unless it already exists.
tryCreateTable :: MonadIO m => Table a -> SeldaT m ()
tryCreateTable = void . flip exec [] . compileCreateTable Ignore

-- | Drop the given table.
dropTable :: MonadIO m => Table a -> SeldaT m ()
dropTable = void . flip exec [] . compileDropTable Fail

-- | Drop the given table, if it exists.
tryDropTable :: MonadIO m => Table a -> SeldaT m ()
tryDropTable = void . flip exec [] . compileDropTable Ignore

-- | Build the final result from a list of result columns.
queryWith :: forall s m a. (MonadIO m, Result a)
          => QueryRunner (Int, [[SqlValue]]) -> Query s a -> m [Res a]
queryWith qr =
  liftIO . fmap (mkResults (Proxy :: Proxy a) . snd) . uncurry qr . compile

-- | Generate the final result of a query from a list of untyped result rows.
mkResults :: Result a => Proxy a -> [[SqlValue]] -> [Res a]
mkResults p = map (toRes p)

-- | Execute a statement without a result.
exec :: MonadIO m => Text -> [Param] -> SeldaT m Int
exec q ps = S $ do
  backend <- ask
  fmap fst . liftIO $ runStmt backend q ps
