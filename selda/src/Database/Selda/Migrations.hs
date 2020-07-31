{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
-- | Functionality for upgrading a table from one schema to another.
module Database.Selda.Migrations
  ( Migration (..)
  , migrate, migrateM, migrateAll, autoMigrate
  ) where
import Control.Monad (void, when)
import Control.Monad.Catch
import Database.Selda hiding (from)
import Database.Selda.Frontend
  ( OnError (..)
  , createTableWithoutIndexes, createTableIndexes
  )
import Database.Selda.Backend.Internal
import Database.Selda.Table.Validation (ValidationError (..))
import Database.Selda.Types (mkTableName, fromTableName, rawTableName)
import Database.Selda.Validation

-- | Wrapper for user with 'migrateAll', enabling multiple migrations to be
--   packed into the same list:
--
-- > migrateAll
-- >   [ Migration m1_from m1_to m1_upgrade
-- >   , Migration m2_from m2_to m2_upgrade
-- >   , ...
-- >   ]
data Migration backend where
  Migration :: (Relational a, Relational b)
            => Table a
            -> Table b
            -> (Row backend a -> Query backend (Row backend b))
            -> Migration backend

-- | A migration step is zero or more migrations that need to be performed in
--   a single transaction in order to keep the database consistent.
type MigrationStep backend = [Migration backend]

-- | Migrate the first table into the second, using the given function to
--   migrate all records to the new schema.
--   Both table schemas are validated before starting the migration, and the
--   source table is validated against what's currently in the database.
--
--   The migration is performed as a transaction, ensuring that either the
--   entire migration passes, or none of it does.
migrate :: (MonadSelda m, MonadMask m, Relational a, Relational b)
        => Table a -- ^ Table to migrate from.
        -> Table b -- ^ Table to migrate to.
        -> (Row (Backend m) a -> Row (Backend m) b)
                   -- ^ Mapping from old to new table.
        -> m ()
migrate t1 t2 upg = migrateM t1 t2 (pure . upg)

-- | Like 'migrate', but allows the column upgrade to access
--   the entire database.
migrateM :: (MonadSelda m, MonadMask m, Relational a, Relational b)
         => Table a
         -> Table b
         -> (Row (Backend m) a -> Query (Backend m) (Row (Backend m) b))
         -> m ()
migrateM t1 t2 upg = migrateAll True [Migration t1 t2 upg]

wrap :: (MonadSelda m, MonadMask m) => Bool -> m a -> m a
wrap enforceFKs
  | enforceFKs = transaction
  | otherwise  = withoutForeignKeyEnforcement

-- | Perform all given migrations as a single transaction.
migrateAll :: (MonadSelda m, MonadMask m)
           => Bool -- ^ Enforce foreign keys during migration?
           -> MigrationStep (Backend m) -- ^ Migration step to perform.
           -> m ()
migrateAll fks =
  wrap fks . mapM_ (\(Migration t1 t2 upg) -> migrateInternal t1 t2 upg)

-- | Given a list of migration steps in ascending chronological order, finds
--   the latest migration step starting state that matches the current database,
--   and performs all migrations from that point until the end of the list.
--   The whole operation is performed as a single transaction.
--
--   If no matching starting state is found, a 'ValidationError' is thrown.
--   If the database is already in the state specified by the end state of the
--   final step, no migration is performed.
--
--   Note that when looking for a matching starting state, index methods for
--   indexed columns are not taken into account. Two columns @c1@ and @c2@ are
--   considered to be identical if @c1@ is indexed with index method @foo@ and
--   @c2@ is indexed with index method @bar@.
autoMigrate :: (MonadSelda m, MonadMask m)
            => Bool -- ^ Enforce foreign keys during migration?
            -> [MigrationStep (Backend m)] -- ^ Migration steps to perform.
            -> m ()
autoMigrate _ [] = do
  return ()
autoMigrate fks steps = wrap fks $ do
    diffs <- sequence finalState
    when (any (/= TableOK) diffs) $ do
      steps' <- reverse <$> calculateSteps revSteps
      mapM_ performStep steps'
  where
    revSteps = reverse steps
    finalState = [diffTable to | Migration _ to _ <- head revSteps]

    calculateSteps (step:ss) = do
      diffs <- mapM (\(Migration from _ _) -> diffTable from) step
      if all (== TableOK) diffs
        then return [step]
        else (step:) <$> calculateSteps ss
    calculateSteps [] = do
      throwM $ ValidationError "no starting state matches the current state of the database"

    performStep = mapM_ (\(Migration t1 t2 upg) -> migrateInternal t1 t2 upg)

-- | Workhorse for migration.
--   Is NOT performed as a transaction, so exported functions need to
--   properly wrap calls this function.
migrateInternal :: (MonadSelda m, MonadThrow m, Relational a, Relational b)
                => Table a
                -> Table b
                -> (Row (Backend m) a -> Query (Backend m) (Row (Backend m) b))
                -> m ()
migrateInternal t1 t2 upg = withBackend $ \b -> do
    validateTable t1
    validateSchema t2
    createTableWithoutIndexes Fail t2'
    void . queryInto t2' $ select t1 >>= upg
    void . liftIO $ runStmt b (dropQuery (tableName t1)) []
    void . liftIO $ runStmt b renameQuery []
    createTableIndexes Fail t2
  where
    t2' = t2 {tableName = mkTableName newName} `asTypeOf` t2
    newName = mconcat ["__selda_migration_", rawTableName (tableName t2)]
    renameQuery = mconcat
      [ "ALTER TABLE ", newName
      , " RENAME TO ", fromTableName (tableName t2), ";"
      ]
    dropQuery t = mconcat ["DROP TABLE ", fromTableName t, ";"]
