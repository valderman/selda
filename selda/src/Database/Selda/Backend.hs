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
import Control.Monad ( unless )
import Control.Monad.Catch ( mask_ )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.IORef ( atomicModifyIORef' )
import Database.Selda.Backend.Internal
    ( Param(..),
      ColAttr(..),
      AutoIncType(..),
      isAutoPrimary,
      isPrimary,
      isUnique,
      PPConfig(..),
      defPPConfig,
      SeldaM,
      SeldaT,
      MonadSelda(..),
      SeldaBackend(..),
      ColumnInfo(..),
      TableInfo(..),
      SeldaConnection(connClosed, connBackend),
      QueryRunner,
      StmtID,
      SeldaError(..),
      BackendID(..),
      newConnection,
      allStmts,
      fromColInfo,
      tableInfo,
      withBackend,
      runSeldaT )
import Database.Selda.SqlType as SqlType
    ( UUID,
      UUID'(..),
      ID(..),
      RowID,
      SqlValue(..),
      Lit(..),
      SqlEnum(..),
      SqlType(..),
      SqlTypeRep(..),
      sqlDateTimeFormat,
      sqlDateFormat,
      sqlTimeFormat,
      litType,
      compLit,
      invalidRowId,
      isInvalidRowId,
      toRowId,
      fromRowId,
      typedUuid,
      toId,
      fromId,
      invalidId,
      isInvalidId )
import Database.Selda.Table.Type ( IndexMethod(..) )
import Database.Selda.Types
    ( TableName,
      ColName,
      fromColName,
      fromTableName,
      rawTableName,
      mkColName,
      mkTableName )

-- | Close a reusable Selda connection.
--   Closing a connection while in use is undefined.
--   Passing a closed connection to 'runSeldaT' results in a 'SeldaError'
--   being thrown. Closing a connection more than once is a no-op.
seldaClose :: MonadIO m => SeldaConnection b -> m ()
seldaClose c = liftIO $ mask_ $ do
  closed <- atomicModifyIORef' (connClosed c) $ \closed -> (True, closed)
  unless closed $ closeConnection (connBackend c) c
