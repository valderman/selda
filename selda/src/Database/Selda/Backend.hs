-- | API for building Selda backends and adding support for more types
--   in queries.
module Database.Selda.Backend
  ( MonadSelda (..), SeldaT, SeldaM, SeldaError (..)
  , StmtID, BackendID (..), QueryRunner, SeldaBackend (..), SeldaConnection
  , SqlValue (..)
  , IndexMethod (..)
  , Param (..), ColAttr (..), AutoIncType (..)
  , PPConfig (..), defPPConfig
  , TableName, ColName, TableInfo (..), ColumnInfo (..)
  , isAutoPrimary, isPrimary, isUnique
  , tableInfo, fromColInfo
  , mkTableName, mkColName, fromTableName, fromColName, rawTableName
  , newConnection, allStmts, withBackend
  , runSeldaT, seldaClose
  , module SqlType
  ) where
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Database.Selda.Backend.Internal
import Database.Selda.SqlType as SqlType
import Database.Selda.Table (IndexMethod (..))
import Database.Selda.Types

-- | Close a reusable Selda connection.
--   Closing a connection while in use is undefined.
--   Passing a closed connection to 'runSeldaT' results in a 'SeldaError'
--   being thrown. Closing a connection more than once is a no-op.
seldaClose :: MonadIO m => SeldaConnection b -> m ()
seldaClose c = liftIO $ mask_ $ do
  closed <- atomicModifyIORef' (connClosed c) $ \closed -> (True, closed)
  unless closed $ closeConnection (connBackend c) c
