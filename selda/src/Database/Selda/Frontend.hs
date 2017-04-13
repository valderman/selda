{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}
-- | API for running Selda operations over databases.
module Database.Selda.Frontend
  ( Result, Res, MonadIO (..), MonadSelda (..), SeldaT
  , query
  , insert, insert_, insertWithPK
  , update, update_
  , deleteFrom, deleteFrom_
  , createTable, tryCreateTable
  , dropTable, tryDropTable
  , transaction, setLocalCache
  ) where
import Database.Selda.Backend
import Database.Selda.Caching
import Database.Selda.Column
import Database.Selda.Compile
import Database.Selda.Query.Type
import Database.Selda.Table
import Database.Selda.Table.Compile
import Data.Proxy
import Data.Text (Text)
import Control.Monad
import Control.Monad.Catch

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
insert _ [] = do
  return 0
insert t cs = do
  updateLocalCache $ invalidate (tableName t)
  uncurry exec $ compileInsert t cs

-- | Like 'insert', but does not return anything.
--   Use this when you really don't care about how many rows were inserted.
insert_ :: (MonadSelda m, Insert (InsertCols a))
        => Table a -> [InsertCols a] -> m ()
insert_ t cs = void $ insert t cs

-- | Like 'insert', but returns the primary key of the last inserted row.
--   Attempting to run this operation on a table without an auto-incrementing
--   primary key is a type error.
insertWithPK :: (MonadSelda m, HasAutoPrimary a, Insert (InsertCols a))
                => Table a -> [InsertCols a] -> m Int
insertWithPK t cs = do
  backend <- seldaBackend
  updateLocalCache $ invalidate (tableName t)
  liftIO . uncurry (runStmtWithPK backend) $ compileInsert t cs

-- | Update the given table using the given update function, for all rows
--   matching the given predicate. Returns the number of updated rows.
update :: (MonadSelda m, Columns (Cols s a), Result (Cols s a))
       => Table a                  -- ^ The table to update.
       -> (Cols s a -> Col s Bool) -- ^ Predicate.
       -> (Cols s a -> Cols s a)   -- ^ Update function.
       -> m Int
update tbl check upd = do
  updateLocalCache $ invalidate (tableName tbl)
  uncurry exec $ compileUpdate tbl upd check

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
deleteFrom tbl f = do
  updateLocalCache $ invalidate (tableName tbl)
  uncurry exec $ compileDelete tbl f

-- | Like 'deleteFrom', but does not return the number of deleted rows.
deleteFrom_ :: (MonadSelda m, Columns (Cols s a))
            => Table a -> (Cols s a -> Col s Bool) -> m ()
deleteFrom_ tbl f = void $ deleteFrom tbl f

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
dropTable = withInval $ void . flip exec [] . compileDropTable Fail

-- | Drop the given table, if it exists.
tryDropTable :: MonadSelda m => Table a -> m ()
tryDropTable = withInval $ void . flip exec [] . compileDropTable Ignore

-- | Perform the given computation atomically.
--   If an exception is raised during its execution, the enture transaction
--   will be rolled back, and the exception re-thrown.
transaction :: (MonadSelda m, MonadThrow m, MonadCatch m) => m a -> m a
transaction m = do
  void $ exec "BEGIN TRANSACTION" []
  res <- try m
  case res of
    Left (SomeException e) -> do
      void $ exec "ROLLBACK" []
      throwM e
    Right x -> do
      void $ exec "COMMIT" []
      return x

-- | Set the maximum local cache size to @n@. A cache size of zero disables
--   local cache altogether. Changing the cache size will also flush all
--   entries.
--
--   By default, local caching is turned off.
--
--   WARNING: local caching is guaranteed to be consistent with the underlying
--   database, ONLY under the assumption that no other process will modify it.
setLocalCache :: MonadSelda m => Int -> m ()
setLocalCache = updateLocalCache . setMaxItems

-- | Build the final result from a list of result columns.
queryWith :: forall s m a. (MonadSelda m, Result a)
          => QueryRunner (Int, [[SqlValue]]) -> Query s a -> m [Res a]
queryWith qr q = do
    mres <- cached qry <$> getLocalCache
    mis <- maxItems <$> getLocalCache
    case mres of
      (Just res, c') -> do
        updateLocalCache $ const c'
        return res
      _        -> do
        res <- fmap snd . liftIO $ uncurry qr qry
        let res' = mkResults (Proxy :: Proxy a) res
        updateLocalCache $ cache tables qry res'
        return res'
  where
    (tables, qry) = compileWithTables q

-- | Generate the final result of a query from a list of untyped result rows.
mkResults :: Result a => Proxy a -> [[SqlValue]] -> [Res a]
mkResults p = map (toRes p)

-- | Run the given computation over a table after invalidating all cached
--   results depending on that table.
withInval :: MonadSelda m => (Table a -> m b) -> Table a -> m b
withInval f t = do
  updateLocalCache (invalidate $ tableName t)
  f t

-- | Execute a statement without a result.
exec :: MonadSelda m => Text -> [Param] -> m Int
exec q ps = do
  backend <- seldaBackend
  fmap fst . liftIO $ runStmt backend q ps
