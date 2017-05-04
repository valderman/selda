{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | API for building Selda backends.
module Database.Selda.Backend
  ( MonadIO (..)
  , QueryRunner, SeldaBackend (..), MonadSelda (..), SeldaT (..), SeldaM
  , SeldaError (..)
  , Param (..), Lit (..), SqlValue (..), ColAttr (..)
  , compileColAttr
  , sqlDateTimeFormat, sqlDateFormat, sqlTimeFormat
  , runSeldaT
  ) where
import Database.Selda.Caching (invalidate)
import Database.Selda.SQL (Param (..))
import Database.Selda.SqlType
import Database.Selda.Table (Table, ColAttr (..), tableName)
import Database.Selda.Table.Compile (compileColAttr)
import Database.Selda.Types (TableName)
import Control.Exception (throwIO)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Text (Text)
import Data.Typeable

-- | Thrown by any function in 'SeldaT' if an error occurs.
data SeldaError
  = DbError String  -- ^ Unable to open or connect to database.
  | SqlError String -- ^ An error occurred while executing query.
  deriving (Show, Eq, Typeable)

instance Exception SeldaError

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

    -- | Generate a custom column type for the column having the given Selda
    --   type and list of attributes.
  , customColType :: Text -> [ColAttr] -> Maybe Text

    -- | The keyword that represents the default value for auto-incrementing
    --   primary keys.
  , defaultKeyword :: Text

    -- | A string uniquely identifying the database used by this invocation
    --   of the backend. This could be, for instance, a PostgreSQL connection
    --   string or the absolute path to an SQLite file.
  , dbIdentifier   :: Text
}

data SeldaState = SeldaState
  { -- | Backend in use by the current computation.
    stBackend :: !SeldaBackend

    -- | Tables modified by the current transaction.
    --   Invariant: always @Just xs@ during a transaction, and always
    --   @Nothing@ when not in a transaction.
  , stTouchedTables :: !(Maybe [TableName])
  }

-- | Some monad with Selda SQL capabilitites.
class MonadIO m => MonadSelda m where
  -- | Get the backend in use by the computation.
  seldaBackend :: m SeldaBackend

  -- | Invalidate the given table as soon as the current transaction finishes.
  --   Invalidate the table immediately if no transaction is ongoing.
  invalidateTable :: Table a -> m ()

  -- | Indicates the start of a new transaction.
  --   Starts bookkeeping to invalidate all tables modified during
  --   the transaction at the next call to 'endTransaction'.
  beginTransaction :: m ()

  -- | Indicates the end of the current transaction.
  --   Invalidates all tables that were modified since the last call to
  --   'beginTransaction', unless the transaction was rolled back.
  endTransaction :: Bool -- ^ @True@ if the transaction was committed,
                         --   @False@ if it was rolled back.
                 -> m ()

-- | Monad transformer adding Selda SQL capabilities.
newtype SeldaT m a = S {unS :: StateT SeldaState m a}
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask, MonadTrans
           )

instance MonadIO m => MonadSelda (SeldaT m) where
  seldaBackend = S $ fmap stBackend get

  invalidateTable tbl = S $ do
    st <- get
    case stTouchedTables st of
      Nothing -> liftIO $ invalidate [tableName tbl]
      Just ts -> put $ st {stTouchedTables = Just (tableName tbl : ts)}

  beginTransaction = S $ do
    st <- get
    case stTouchedTables st of
      Nothing -> put $ st {stTouchedTables = Just []}
      Just _  -> liftIO $ throwIO $ SqlError "attempted to nest transactions"

  endTransaction committed = S $ do
    st <- get
    case stTouchedTables st of
      Just ts | committed -> liftIO $ invalidate ts
      _                   -> return ()
    put $ st {stTouchedTables = Nothing}

-- | The simplest form of Selda computation; 'SeldaT' specialized to 'IO'.
type SeldaM = SeldaT IO

-- | Run a Selda transformer. Backends should use this to implement their
--   @withX@ functions.
runSeldaT :: MonadIO m => SeldaT m a -> SeldaBackend -> m a
runSeldaT m b = fst <$> runStateT (unS m) (SeldaState b Nothing)
