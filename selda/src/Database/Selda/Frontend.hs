{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
-- | API for running Selda operations over databases.
module Database.Selda.Frontend
  ( Result, Res, MonadIO (..), MonadSelda (..), SeldaT
  , query
  , insert, insert_, insertWithPK
  , update, update_
  , deleteFrom, deleteFrom_
  , createTable, tryCreateTable
  , dropTable, tryDropTable
  ) where
import Database.Selda.Backend
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

-- | Run a query within a Selda transformer.
--   Selda transformers are entered using backend-specific @withX@ functions,
--   such as 'withSQLite' from the SQLite backend.
query :: (MonadSelda m, Result a) => Query s a -> m [Res a]
query q = do
  backend <- seldaBackend
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
insert :: (MonadSelda m, Insert (InsertCols a))
       => Table a -> [InsertCols a] -> m Int
insert _ [] = return 0
insert t cs = uncurry exec $ compileInsert t cs

-- | Like 'insert', but does not return anything.
--   Use this when you really don't care about how many rows were inserted.
insert_ :: (MonadSelda m, Insert (InsertCols a))
        => Table a -> [InsertCols a] -> m ()
insert_ t cs = void $ insert t cs

-- | Like 'insert', but returns the primary key of the last inserted row.
--   Attempting 
insertWithPK :: (MonadSelda m, HasAutoPrimary a, Insert (InsertCols a))
                => Table a -> [InsertCols a] -> m Int
insertWithPK t cs = do
  backend <- seldaBackend
  liftIO . uncurry (runStmtWithPK backend) $ compileInsert t cs

-- | Update the given table using the given update function, for all rows
--   matching the given predicate. Returns the number of updated rows.
update :: (MonadSelda m, Columns (Cols s a), Result (Cols s a))
       => Table a                  -- ^ The table to update.
       -> (Cols s a -> Col s Bool) -- ^ Predicate.
       -> (Cols s a -> Cols s a)   -- ^ Update function.
       -> m Int
update tbl check upd = uncurry exec $ compileUpdate tbl upd check

-- | Like 'update', but doesn't return the number of updated rows.
update_ :: (MonadSelda m, Columns (Cols s a), Result (Cols s a))
       => Table a
       -> (Cols s a -> Col s Bool)
       -> (Cols s a -> Cols s a)
       -> m ()
update_ tbl check upd = void $ update tbl check upd

-- | From the given table, delete all rows matching the given predicate.
--   Returns the number of deleted rows.
deleteFrom :: (MonadSelda m, Columns (Cols s a))
           => Table a -> (Cols s a -> Col s Bool) -> m Int
deleteFrom tbl f = uncurry exec $ compileDelete tbl f

-- | Like 'deleteFrom', but does not return the number of deleted rows.
deleteFrom_ :: (MonadSelda m, Columns (Cols s a))
            => Table a -> (Cols s a -> Col s Bool) -> m ()
deleteFrom_ tbl f = void . uncurry exec $ compileDelete tbl f

-- | Create a table from the given schema.
createTable :: MonadSelda m => Table a -> m ()
createTable tbl = do
  cct <- customColType <$> seldaBackend
  void . flip exec [] $ compileCreateTable cct Fail tbl

-- | Create a table from the given schema, unless it already exists.
tryCreateTable :: MonadSelda m => Table a -> m ()
tryCreateTable tbl = do
  cct <- customColType <$> seldaBackend
  void . flip exec [] $ compileCreateTable cct Ignore tbl

-- | Drop the given table.
dropTable :: MonadSelda m => Table a -> m ()
dropTable = void . flip exec [] . compileDropTable Fail

-- | Drop the given table, if it exists.
tryDropTable :: MonadSelda m => Table a -> m ()
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
exec :: MonadSelda m => Text -> [Param] -> m Int
exec q ps = do
  backend <- seldaBackend
  fmap fst . liftIO $ runStmt backend q ps
