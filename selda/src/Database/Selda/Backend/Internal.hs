{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Internal backend API.
module Database.Selda.Backend.Internal
  ( StmtID, BackendID (..)
  , QueryRunner, SeldaBackend (..), SeldaConnection (..), SeldaStmt (..)
  , MonadSelda (..), SeldaT (..), SeldaM
  , SeldaError (..)
  , Param (..), Lit (..), ColAttr (..)
  , SqlType (..), SqlValue (..), SqlTypeRep (..)
  , PPConfig (..), defPPConfig
  , ColumnInfo (..), columnInfo, fromColInfo
  , sqlDateTimeFormat, sqlDateFormat, sqlTimeFormat
  , freshStmtId
  , invalidate
  , newConnection, allStmts
  , runSeldaT, seldaBackend
  ) where
import Database.Selda.Caching (invalidate, setMaxItems)
import Database.Selda.SQL (Param (..))
import Database.Selda.SqlType
import Database.Selda.Table (Table (..), ColAttr (..), tableName)
import qualified Database.Selda.Table as Table (ColInfo (..))
import Database.Selda.SQL.Print.Config
import Database.Selda.Types (TableName, ColName)
import Control.Concurrent
import Control.Exception (throw)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Dynamic
import Data.Hashable
import qualified Data.HashMap.Strict as M
import Data.IORef
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

-- | Uniquely identifies some particular backend.
--
--   When publishing a new backend, consider submitting a pull request with a
--   constructor for your backend instead of using the @Other@ constructor.
data BackendID = SQLite | PostgreSQL | Other Text
  deriving (Show, Eq, Ord)

-- | Thrown by any function in 'SeldaT' if an error occurs.
data SeldaError
  = DbError String  -- ^ Unable to open or connect to database.
  | SqlError String -- ^ An error occurred while executing query.
  deriving (Show, Eq, Typeable)

instance Exception SeldaError

-- | A prepared statement identifier. Guaranteed to be unique per application.
newtype StmtID = StmtID Int
  deriving (Show, Eq, Ord, Hashable)

-- | A connection identifier. Guaranteed to be unique per application.
newtype ConnID = ConnID Int
  deriving (Show, Eq, Ord)

{-# NOINLINE nextStmtId #-}
nextStmtId :: IORef Int
nextStmtId = unsafePerformIO $ newIORef 1

-- | Generate a fresh statement identifier, guaranteed to be unique per process.
freshStmtId :: MonadIO m => m StmtID
freshStmtId = liftIO $ atomicModifyIORef' nextStmtId $ \n -> (n+1, StmtID n)

-- | A function which executes a query and gives back a list of extensible
--   tuples; one tuple per result row, and one tuple element per column.
type QueryRunner a = Text -> [Param] -> IO a

-- | A prepared statement.
data SeldaStmt = SeldaStmt
 { -- | Backend-specific handle to the prepared statement.
   stmtHandle :: !Dynamic

   -- | The SQL code for the statement.
 , stmtText :: !Text

   -- | All parameters to be passed to the prepared statement.
   --   Parameters that are unique to each invocation are specified as indices
   --   starting at 0.
   --   Backends implementing @runPrepared@ should probably ignore this field.
 , stmtParams :: ![Either Int Param]

   -- | All tables touched by the statement.
 , stmtTables :: ![TableName]
 }

data SeldaConnection = SeldaConnection
  { -- | The backend used by the current connection.
    connBackend :: !SeldaBackend

    -- | A string uniquely identifying the database used by this connection.
    --   This could be, for instance, a PostgreSQL connection
    --   string or the absolute path to an SQLite file.
  , connDbId :: Text

    -- | All statements prepared for this connection.
  , connStmts :: !(IORef (M.HashMap StmtID SeldaStmt))

    -- | Is the connection closed?
  , connClosed :: !(IORef Bool)

    -- | Lock to prevent this connection from being used concurrently by
    --   multiple invocations of 'runSeldaT'.
  , connLock :: !(MVar ())
}

-- | Create a new Selda connection for the given backend and database
--   identifier string.
newConnection :: MonadIO m => SeldaBackend -> Text -> m SeldaConnection
newConnection back dbid =
  liftIO $ SeldaConnection back dbid <$> newIORef M.empty
                                     <*> newIORef False
                                     <*> newMVar ()

-- | Get all statements and their corresponding identifiers for the current
--   connection.
allStmts :: SeldaConnection -> IO [(StmtID, Dynamic)]
allStmts =
  fmap (map (\(k, v) -> (k, stmtHandle v)) . M.toList) . readIORef . connStmts

-- | Comprehensive information about a column.
data ColumnInfo = ColumnInfo
  { -- | Name of the column.
    colName :: ColName
    -- | Selda type of the column, or the type name given by the database
    --   if Selda couldn't make sense of the type.
  , colType :: Either Text SqlTypeRep
    -- | Is the given column the primary key of its table?
  , colIsPK :: Bool
    -- | Is the given column auto-incrementing?
  , colIsAutoIncrement :: Bool
    -- | Is the column unique, either through a UNIQUE constraint or by virtue
    --   of being a primary key?
  , colIsUnique :: Bool
    -- | Can the column be NULL?
  , colIsNullable :: Bool
    -- | Does the column have an index?
  , colHasIndex :: Bool
    -- | Any foreign key (table, column) pairs referenced by this column.
  , colFKs :: [(TableName, ColName)]
  } deriving (Show, Eq)

-- | Convert a 'Table.ColInfo' into a 'ColumnInfo'.
fromColInfo :: Table.ColInfo -> ColumnInfo
fromColInfo ci = ColumnInfo
    { colName = Table.colName ci
    , colType = Right $ Table.colType ci
    , colIsPK = Primary `elem` Table.colAttrs ci
    , colIsAutoIncrement = AutoIncrement `elem` Table.colAttrs ci
    , colIsUnique = Unique `elem` Table.colAttrs ci
    , colIsNullable = Optional `elem` Table.colAttrs ci
    , colHasIndex = not $ null [() | Indexed _ <- Table.colAttrs ci]
    , colFKs = map fk (Table.colFKs ci)
    }
  where
    fk (Table tbl _ _, col) = (tbl, col)

-- | Get the column information for each column in the given table.
columnInfo :: Table a -> [ColumnInfo]
columnInfo = map fromColInfo . tableCols

-- | A collection of functions making up a Selda backend.
data SeldaBackend = SeldaBackend
  { -- | Execute an SQL statement.
    runStmt :: Text -> [Param] -> IO (Int, [[SqlValue]])

    -- | Execute an SQL statement and return the last inserted primary key,
    --   where the primary key is auto-incrementing.
    --   Backends must take special care to make this thread-safe.
  , runStmtWithPK :: Text -> [Param] -> IO Int

    -- | Prepare a statement using the given statement identifier.
  , prepareStmt :: StmtID -> [SqlTypeRep] -> Text -> IO Dynamic

    -- | Execute a prepared statement.
  , runPrepared :: Dynamic -> [Param] -> IO (Int, [[SqlValue]])

    -- | Get a list of all columns in the given table, with the type and any
    --   modifiers for each column.
    --   Return an empty list if the given table does not exist.
  , getTableInfo :: TableName -> IO [ColumnInfo]

    -- | SQL pretty-printer configuration.
  , ppConfig :: PPConfig

    -- | Close the currently open connection.
  , closeConnection :: SeldaConnection -> IO ()

    -- | Unique identifier for this backend.
  , backendId :: BackendID

    -- | Turn on or off foreign key checking, and initiate/commit
    --   a transaction.
    --
    --   When implementing this function, it is safe to assume that
    --   @disableForeignKeys True@
    --   will always be called exactly once before each
    --   @disableForeignKeys False@.
  , disableForeignKeys :: Bool -> IO ()
  }

data SeldaState = SeldaState
  { -- | Connection in use by the current computation.
    stConnection :: !SeldaConnection

    -- | Tables modified by the current transaction.
    --   Invariant: always @Just xs@ during a transaction, and always
    --   @Nothing@ when not in a transaction.
  , stTouchedTables :: !(Maybe [TableName])
  }

-- | Some monad with Selda SQL capabilitites.
--
--   Note that the default implementations of 'invalidateTable' and
--   'wrapTransaction' flush the entire cache and disable caching when
--   invoked. If you want to use Selda's built-in caching mechanism, you will
--   need to implement these operations yourself.
class (MonadIO m, MonadMask m) => MonadSelda m where
  -- | Get the connection in use by the computation.
  seldaConnection :: m SeldaConnection

  -- | Invalidate the given table as soon as the current transaction finishes.
  --   Invalidate the table immediately if no transaction is ongoing.
  invalidateTable :: Table a -> m ()
  invalidateTable _ = liftIO $ setMaxItems 0

  -- | Safely wrap a transaction. To ensure consistency of the in-process cache,
  --   it is important that any cached tables modified during a transaction are
  --   invalidated ONLY if that transaction succeeds, AFTER the changes become
  --   visible in the database.
  --
  --   In order to be thread-safe in the presence of asynchronous exceptions,
  --   instances should:
  --
  --   1. Mask async exceptions.
  --   2. Start bookkeeping of tables invalidated during the transaction.
  --   3. Perform the transaction, with async exceptions restored.
  --   4. Commit transaction, invalidate tables, and disable bookkeeping; OR
  --   5. If an exception was raised, rollback transaction,
  --      disable bookkeeping, and re-throw the exception.
  wrapTransaction :: m () -- ^ Signal transaction commit to SQL backend.
                  -> m () -- ^ Signal transaction rollback to SQL backend.
                  -> m a  -- ^ Transaction to perform.
                  -> m a
  wrapTransaction commit rollback act = do
    bracketOnError (pure ())
                   (const rollback)
                   (const (act <* commit <* liftIO (setMaxItems 0)))
  {-# MINIMAL seldaConnection #-}

-- | Get the backend in use by the computation.
seldaBackend :: MonadSelda m => m SeldaBackend
seldaBackend = connBackend <$> seldaConnection

-- | Monad transformer adding Selda SQL capabilities.
newtype SeldaT m a = S {unS :: StateT SeldaState m a}
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask, MonadTrans
           )

instance (MonadIO m, MonadMask m) => MonadSelda (SeldaT m) where
  seldaConnection = S $ fmap stConnection get

  invalidateTable tbl = S $ do
    st <- get
    case stTouchedTables st of
      Nothing -> liftIO $ invalidate [tableName tbl]
      Just ts -> put $ st {stTouchedTables = Just (tableName tbl : ts)}

  wrapTransaction commit rollback m = mask $ \restore -> do
    S $ modify' $ \st ->
      case stTouchedTables st of
        Nothing -> st {stTouchedTables = Just []}
        Just _  -> throw $ SqlError "attempted to nest transactions"
    x <- restore m `onException` rollback
    commit
    st <- S get
    maybe (return ()) (liftIO . invalidate) (stTouchedTables st)
    S $ put $ st {stTouchedTables = Nothing}
    return x

-- | The simplest form of Selda computation; 'SeldaT' specialized to 'IO'.
type SeldaM = SeldaT IO

-- | Run a Selda transformer. Backends should use this to implement their
--   @withX@ functions.
runSeldaT :: (MonadIO m, MonadMask m) => SeldaT m a -> SeldaConnection -> m a
runSeldaT m c =
    bracket (liftIO $ takeMVar (connLock c))
            (const $ liftIO $ putMVar (connLock c) ())
            (const go)
  where
    go = do
      closed <- liftIO $ readIORef (connClosed c)
      when closed $ do
        liftIO $ throwM $ DbError "runSeldaT called with a closed connection"
      fst <$> runStateT (unS m) (SeldaState c Nothing)
