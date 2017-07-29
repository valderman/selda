{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}
-- | API for running Selda operations over databases.
module Database.Selda.Frontend
  ( Result, Res, MonadIO (..), MonadSelda (..), SeldaT
  , query
  , insert, insert_, insertWithPK, tryInsert, insertWhen, insertUnless
  , update, update_, upsert
  , deleteFrom, deleteFrom_
  , createTable, tryCreateTable
  , dropTable, tryDropTable
  , transaction, setLocalCache
  ) where
import Database.Selda.Backend.Internal
import Database.Selda.Caching
import Database.Selda.Column
import Database.Selda.Compile
import Database.Selda.Query.Type
import Database.Selda.SQL
import Database.Selda.SqlType (RowID, invalidRowId, unsafeRowId)
import Database.Selda.Table
import Database.Selda.Table.Compile
import Data.Proxy
import Data.Text (Text)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

-- | Run a query within a Selda monad. In practice, this is often a 'SeldaT'
--   transformer on top of some other monad.
--   Selda transformers are entered using backend-specific @withX@ functions,
--   such as 'withSQLite' from the SQLite backend.
query :: (MonadSelda m, Result a) => Query s a -> m [Res a]
query q = do
  backend <- seldaBackend
  queryWith (runStmt backend) q

-- | Insert the given values into the given table. All columns of the table
--   must be present. If your table has an auto-incrementing primary key,
--   use the special value 'def' for that column to get the auto-incrementing
--   behavior.
--   Returns the number of rows that were inserted.
--
--   To insert a list of tuples into a table with auto-incrementing primary key:
--
-- > people :: Table (Int :*: Text :*: Int :*: Maybe Text)
-- > people = table "ppl"
-- >        $   autoPrimary "id"
-- >        :*: required "name"
-- >        :*: required "age"
-- >        :*: optional "pet"
-- >
-- > main = withSQLite "my_database.sqlite" $ do
-- >   insert_ people
-- >     [ def :*: "Link"  :*: 125 :*: Just "horse"
-- >     , def :*: "Zelda" :*: 119 :*: Nothing
-- >     , ...
-- >     ]
--
--   Note that if one or more of the inserted rows would cause a constraint
--   violation, NO rows will be inserted; the whole insertion fails atomically.
insert :: (MonadSelda m, Insert a) => Table a -> [a] -> m Int
insert _ [] = do
  return 0
insert t cs = do
  cfg <- ppConfig <$> seldaBackend
  res <- mapM (uncurry exec) $ compileInsert cfg t cs
  invalidateTable t
  return (sum res)

-- | Attempt to insert a list of rows into a table, but don't raise an error
--   if the insertion fails. Returns @True@ if the insertion succeeded, otherwise
--   @False@.
--
--   Like 'insert', if even one of the inserted rows would cause a constraint
--   violation, the whole insert operation fails.
tryInsert :: (MonadCatch m, MonadSelda m, Insert a) => Table a -> [a] -> m Bool
tryInsert tbl row = do
  mres <- try $ insert tbl row
  case mres of
    Right _           -> return True
    Left (SqlError _) -> return False
    Left e            -> throwM e

-- | Attempt to perform the given update. If no rows were updated, insert the
--   given row.
--   Returns the primary key of the inserted row, if the insert was performed.
--   Calling this function on a table which does not have a primary key will
--   return @Just id@ on a successful insert, where @id@ is a row identifier
--   guaranteed to not match any row in any table.
--
--   Note that this may perform two separate queries: one update, potentially
--   followed by one insert.
upsert :: ( MonadCatch m
          , MonadSelda m
          , Insert a
          , Columns (Cols s a)
          , Result (Cols s a)
          )
       => Table a
       -> (Cols s a -> Col s Bool)
       -> (Cols s a -> Cols s a)
       -> [a]
       -> m (Maybe RowID)
upsert tbl check upd rows = transaction $ do
  updated <- update tbl check upd
  if updated == 0
    then Just <$> insertWithPK tbl rows
    else pure Nothing

-- | Perform the given insert, if no rows already present in the table match
--   the given predicate.
--   Returns the primary key of the last inserted row,
--   if the insert was performed.
--   If called on a table which doesn't have an auto-incrementing primary key,
--   @Just id@ is always returned on successful insert, where @id@ is a row
--   identifier guaranteed to not match any row in any table.
insertUnless :: ( MonadCatch m
                , MonadSelda m
                , Insert a
                , Columns (Cols s a)
                , Result (Cols s a)
                )
             => Table a
             -> (Cols s a -> Col s Bool)
             -> [a]
             -> m (Maybe RowID)
insertUnless tbl check rows = upsert tbl check id rows

-- | Like 'insertUnless', but performs the insert when at least one row matches
--   the predicate.
insertWhen :: ( MonadCatch m
              , MonadSelda m
              , Insert a
              , Columns (Cols s a)
              , Result (Cols s a)
              )
           => Table a
           -> (Cols s a -> Col s Bool)
           -> [a]
           -> m (Maybe RowID)
insertWhen tbl check rows = transaction $ do
  matches <- update tbl check id
  if matches > 0
    then Just <$> insertWithPK tbl rows
    else pure Nothing

-- | Like 'insert', but does not return anything.
--   Use this when you really don't care about how many rows were inserted.
insert_ :: (MonadSelda m, Insert a) => Table a -> [a] -> m ()
insert_ t cs = void $ insert t cs

-- | Like 'insert', but returns the primary key of the last inserted row.
--   Attempting to run this operation on a table without an auto-incrementing
--   primary key will always return a row identifier that is guaranteed to not
--   match any row in any table.
insertWithPK :: (MonadSelda m, Insert a) => Table a -> [a] -> m RowID
insertWithPK t cs = do
  b <- seldaBackend
  if tableHasAutoPK t
    then do
      res <- liftIO $ do
        mapM (uncurry (runStmtWithPK b)) $ compileInsert (ppConfig b) t cs
      invalidateTable t
      return $ unsafeRowId (last res)
    else do
      insert_ t cs
      return invalidRowId

-- | Update the given table using the given update function, for all rows
--   matching the given predicate. Returns the number of updated rows.
update :: (MonadSelda m, Columns (Cols s a), Result (Cols s a))
       => Table a                  -- ^ The table to update.
       -> (Cols s a -> Col s Bool) -- ^ Predicate.
       -> (Cols s a -> Cols s a)   -- ^ Update function.
       -> m Int
update tbl check upd = do
  cfg <- ppConfig <$> seldaBackend
  res <- uncurry exec $ compileUpdate cfg tbl upd check
  invalidateTable tbl
  return res

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
  cfg <- ppConfig <$> seldaBackend
  res <- uncurry exec $ compileDelete cfg tbl f
  invalidateTable tbl
  return res

-- | Like 'deleteFrom', but does not return the number of deleted rows.
deleteFrom_ :: (MonadSelda m, Columns (Cols s a))
            => Table a -> (Cols s a -> Col s Bool) -> m ()
deleteFrom_ tbl f = void $ deleteFrom tbl f

-- | Create a table from the given schema.
createTable :: MonadSelda m => Table a -> m ()
createTable tbl = do
  cfg <- ppConfig <$> seldaBackend
  void . flip exec [] $ compileCreateTable cfg Fail tbl

-- | Create a table from the given schema, unless it already exists.
tryCreateTable :: MonadSelda m => Table a -> m ()
tryCreateTable tbl = do
  cfg <- ppConfig <$> seldaBackend
  void . flip exec [] $ compileCreateTable cfg Ignore tbl

-- | Drop the given table.
dropTable :: MonadSelda m => Table a -> m ()
dropTable = withInval $ void . flip exec [] . compileDropTable Fail

-- | Drop the given table, if it exists.
tryDropTable :: MonadSelda m => Table a -> m ()
tryDropTable = withInval $ void . flip exec [] . compileDropTable Ignore

-- | Perform the given computation atomically.
--   If an exception is raised during its execution, the enture transaction
--   will be rolled back, and the exception re-thrown.
transaction :: (MonadSelda m, MonadCatch m) => m a -> m a
transaction m =
  wrapTransaction (void $ exec "COMMIT" []) (void $ exec "ROLLBACK" []) $ do
    exec "BEGIN TRANSACTION" [] *> m

-- | Set the maximum local cache size to @n@. A cache size of zero disables
--   local cache altogether. Changing the cache size will also flush all
--   entries. Note that the cache is shared among all Selda computations running
--   within the same process.
--
--   By default, local caching is turned off.
--
--   WARNING: local caching is guaranteed to be consistent with the underlying
--   database, ONLY under the assumption that no other process will modify it.
--   Also note that the cache is shared between ALL Selda computations running
--   within the same process.
setLocalCache :: MonadIO m => Int -> m ()
setLocalCache = liftIO . setMaxItems

-- | Build the final result from a list of result columns.
queryWith :: forall s m a. (MonadSelda m, Result a)
          => QueryRunner (Int, [[SqlValue]]) -> Query s a -> m [Res a]
queryWith qr q = do
  conn <- seldaConnection
  let backend = connBackend conn
      db = connDbId conn
      cacheKey = (db, qs, ps)
      (tables, qry@(qs, ps)) = compileWithTables (ppConfig backend) q
  mres <- liftIO $ cached cacheKey
  case mres of
    Just res -> do
      return res
    _        -> do
      res <- fmap snd . liftIO $ uncurry qr qry
      let res' = mkResults (Proxy :: Proxy a) res
      liftIO $ cache tables cacheKey res'
      return res'

-- | Generate the final result of a query from a list of untyped result rows.
mkResults :: Result a => Proxy a -> [[SqlValue]] -> [Res a]
mkResults p = map (toRes p)

-- | Run the given computation over a table after invalidating all cached
--   results depending on that table.
withInval :: MonadSelda m => (Table a -> m b) -> Table a -> m b
withInval f t = do
  res <- f t
  invalidateTable t
  return res

-- | Execute a statement without a result.
exec :: MonadSelda m => Text -> [Param] -> m Int
exec q ps = do
  backend <- seldaBackend
  fmap fst . liftIO $ runStmt backend q ps
