{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}
-- | API for running Selda operations over databases.
module Database.Selda.Frontend
  ( Result, Res, MonadIO (..), MonadSelda (..), SeldaT, OnError (..)
  , query, queryInto
  , insert, insert_, insertWithPK, tryInsert, insertWhen, insertUnless
  , update, update_, upsert
  , deleteFrom, deleteFrom_
  , createTable, tryCreateTable, createTableWithoutIndexes, createTableIndexes
  , dropTable, tryDropTable
  , transaction, withoutForeignKeyEnforcement
  ) where
import Database.Selda.Backend.Internal
import Database.Selda.Column
import Database.Selda.Compile
import Database.Selda.Generic
import Database.Selda.Query.Type
import Database.Selda.SqlType (ID, invalidId, toId)
import Database.Selda.Table
import Database.Selda.Table.Compile
import Database.Selda.Types (fromTableName)
import Data.Proxy
import Data.Text (Text)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

-- | Run a query within a Selda monad. In practice, this is often a 'SeldaT'
--   transformer on top of some other monad.
--   Selda transformers are entered using backend-specific @withX@ functions,
--   such as 'withSQLite' from the SQLite backend.
query :: (MonadSelda m, Result a) => Query (Backend m) a -> m [Res a]
query q = withBackend (flip queryWith q . runStmt)

-- | Perform the given query, and insert the result into the given table.
--   Returns the number of inserted rows.
queryInto :: (MonadSelda m, Relational a)
          => Table a
          -> Query (Backend m) (Row (Backend m) a)
          -> m Int
queryInto tbl q = withBackend $ \b -> do
    let (qry, ps) = compileWith (ppConfig b) q
        qry' = mconcat ["INSERT INTO ", tblName, " ", qry]
    fmap fst . liftIO $ runStmt b qry' ps
  where
    tblName = fromTableName (tableName tbl)

-- | Insert the given values into the given table. All columns of the table
--   must be present. If your table has an auto-incrementing primary key,
--   use the special value 'def' for that column to get the auto-incrementing
--   behavior.
--   Returns the number of rows that were inserted.
--
--   To insert a list of tuples into a table with auto-incrementing primary key:
--
-- > data Person = Person
-- >   { id :: ID Person
-- >   , name :: Text
-- >   , age :: Int
-- >   , pet :: Maybe Text
-- >   } deriving Generic
-- > instance SqlResult Person
-- >
-- > people :: Table Person
-- > people = table "people" [autoPrimary :- id]
-- >
-- > main = withSQLite "my_database.sqlite" $ do
-- >   insert_ people
-- >     [ Person def "Link" 125 (Just "horse")
-- >     , Person def "Zelda" 119 Nothing
-- >     , ...
-- >     ]
--
--   Note that if one or more of the inserted rows would cause a constraint
--   violation, NO rows will be inserted; the whole insertion fails atomically.
insert :: (MonadSelda m, Relational a) => Table a -> [a] -> m Int
insert _ [] = do
  return 0
insert t cs = withBackend $ \b -> do
  sum <$> mapM (uncurry exec) (compileInsert (ppConfig b) t cs)

-- | Attempt to insert a list of rows into a table, but don't raise an error
--   if the insertion fails. Returns @True@ if the insertion succeeded, otherwise
--   @False@.
--
--   Like 'insert', if even one of the inserted rows would cause a constraint
--   violation, the whole insert operation fails.
tryInsert :: (MonadSelda m, MonadCatch m, Relational a) => Table a -> [a] -> m Bool
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
upsert :: (MonadSelda m, MonadMask m, Relational a)
       => Table a
       -> (Row (Backend m) a -> Col (Backend m) Bool)
       -> (Row (Backend m) a -> Row (Backend m) a)
       -> [a]
       -> m (Maybe (ID a))
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
insertUnless :: (MonadSelda m, MonadMask m, Relational a)
             => Table a
             -> (Row (Backend m) a -> Col (Backend m) Bool)
             -> [a]
             -> m (Maybe (ID a))
insertUnless tbl check rows = upsert tbl check id rows

-- | Like 'insertUnless', but performs the insert when at least one row matches
--   the predicate.
insertWhen :: (MonadSelda m, MonadMask m, Relational a)
           => Table a
           -> (Row (Backend m) a -> Col (Backend m) Bool)
           -> [a]
           -> m (Maybe (ID a))
insertWhen tbl check rows = transaction $ do
  matches <- update tbl check id
  if matches > 0
    then Just <$> insertWithPK tbl rows
    else pure Nothing

-- | Like 'insert', but does not return anything.
--   Use this when you really don't care about how many rows were inserted.
insert_ :: (MonadSelda m, Relational a) => Table a -> [a] -> m ()
insert_ t cs = void $ insert t cs

-- | Like 'insert', but returns the primary key of the last inserted row.
--   Attempting to run this operation on a table without an auto-incrementing
--   primary key will always return a row identifier that is guaranteed to not
--   match any row in any table.
insertWithPK :: (MonadSelda m, Relational a) => Table a -> [a] -> m (ID a)
insertWithPK t cs = withBackend $ \b -> do
  if tableHasAutoPK t
    then do
      res <- liftIO $ do
        mapM (uncurry (runStmtWithPK b)) $ compileInsert (ppConfig b) t cs
      return $ toId (last res)
    else do
      insert_ t cs
      return invalidId

-- | Update the given table using the given update function, for all rows
--   matching the given predicate. Returns the number of updated rows.
update :: (MonadSelda m, Relational a)
       => Table a                                     -- ^ Table to update.
       -> (Row (Backend m) a -> Col (Backend m) Bool) -- ^ Predicate.
       -> (Row (Backend m) a -> Row (Backend m) a)    -- ^ Update function.
       -> m Int
update tbl check upd = withBackend $ \b -> do
  res <- uncurry exec $ compileUpdate (ppConfig b) tbl upd check
  return res

-- | Like 'update', but doesn't return the number of updated rows.
update_ :: (MonadSelda m, Relational a)
       => Table a
       -> (Row (Backend m) a -> Col (Backend m) Bool)
       -> (Row (Backend m) a -> Row (Backend m) a)
       -> m ()
update_ tbl check upd = void $ update tbl check upd

-- | From the given table, delete all rows matching the given predicate.
--   Returns the number of deleted rows.
deleteFrom :: (MonadSelda m, Relational a)
           => Table a
           -> (Row (Backend m) a -> Col (Backend m) Bool)
           -> m Int
deleteFrom tbl f = withBackend $ \b -> do
  res <- uncurry exec $ compileDelete (ppConfig b) tbl f
  return res

-- | Like 'deleteFrom', but does not return the number of deleted rows.
deleteFrom_ :: (MonadSelda m, Relational a)
            => Table a
            -> (Row (Backend m) a -> Col (Backend m) Bool)
            -> m ()
deleteFrom_ tbl f = void $ deleteFrom tbl f

-- | Create a table from the given schema.
createTable :: MonadSelda m => Table a -> m ()
createTable tbl = do
  createTableWithoutIndexes Fail tbl
  createTableIndexes Fail tbl

-- | Create a table from the given schema, but don't create any indexes.
createTableWithoutIndexes :: MonadSelda m => OnError -> Table a -> m ()
createTableWithoutIndexes onerror tbl = withBackend $ \b -> do
  void $ exec (compileCreateTable (ppConfig b) onerror tbl) []

-- | Create all indexes for the given table. Fails if any of the table's indexes
--   already exists.
createTableIndexes :: MonadSelda m => OnError -> Table a -> m ()
createTableIndexes ifex tbl = withBackend $ \b -> do
  mapM_ (flip exec []) $ compileCreateIndexes (ppConfig b) ifex tbl

-- | Create a table from the given schema, unless it already exists.
tryCreateTable :: MonadSelda m => Table a -> m ()
tryCreateTable tbl = do
  createTableWithoutIndexes Ignore tbl
  createTableIndexes Ignore tbl

-- | Drop the given table.
dropTable :: MonadSelda m => Table a -> m ()
dropTable = void . flip exec [] . compileDropTable Fail

-- | Drop the given table, if it exists.
tryDropTable :: MonadSelda m => Table a -> m ()
tryDropTable = void . flip exec [] . compileDropTable Ignore

-- | Perform the given computation atomically.
--   If an exception is raised during its execution, the entire transaction
--   will be rolled back and the exception re-thrown, even if the exception
--   is caught and handled within the transaction.
transaction :: (MonadSelda m, MonadMask m) => m a -> m a
transaction m = mask $ \restore -> transact $ do
  void $ exec "BEGIN TRANSACTION" []
  x <- restore m `onException` void (exec "ROLLBACK" [])
  void $ exec "COMMIT" []
  return x

-- | Run the given computation as a transaction without enforcing foreign key
--   constraints.
--
--   If the computation finishes with the database in an inconsistent state
--   with regards to foreign keys, the resulting behavior is undefined.
--   Use with extreme caution, preferably only for migrations.
--
--   On the PostgreSQL backend, at least PostgreSQL 9.6 is required.
--
--   Using this should be avoided in favor of deferred foreign key
--   constraints. See SQL backend documentation for deferred constraints.
withoutForeignKeyEnforcement :: (MonadSelda m, MonadMask m) => m a -> m a
withoutForeignKeyEnforcement m = withBackend $ \b -> do
  bracket_ (liftIO $ disableForeignKeys b True)
           (liftIO $ disableForeignKeys b False)
           m

-- | Build the final result from a list of result columns.
queryWith :: forall m a. (MonadSelda m, Result a)
          => QueryRunner (Int, [[SqlValue]]) -> Query (Backend m) a -> m [Res a]
queryWith run q = withBackend $ \b -> do
  res <- fmap snd . liftIO . uncurry run $ compileWith (ppConfig b) q
  return $ mkResults (Proxy :: Proxy a) res

-- | Generate the final result of a query from a list of untyped result rows.
mkResults :: Result a => Proxy a -> [[SqlValue]] -> [Res a]
mkResults p = map (buildResult p)

{-# INLINE exec #-}
-- | Execute a statement without a result.
exec :: MonadSelda m => Text -> [Param] -> m Int
exec q ps = withBackend $ \b -> liftIO $ execIO b q ps

{-# INLINE execIO #-}
-- | Like 'exec', but in 'IO'.
execIO :: SeldaBackend b -> Text -> [Param] -> IO Int
execIO backend q ps = fmap fst $ runStmt backend q ps
