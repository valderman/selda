-- | API for building Selda backends and adding support for more types
--   in queries.
module Database.Selda.Backend
  ( MonadSelda (..), SeldaT, SeldaM, SeldaError (..)
  , StmtID, BackendID (..), QueryRunner, SeldaBackend (..), SeldaConnection
  , SqlType (..), SqlValue (..), SqlTypeRep (..)
  , Param (..), Lit (..), ColAttr (..)
  , PPConfig (..), defPPConfig
  , newConnection, allStmts, seldaBackend
  , runSeldaT, seldaClose
  , sqlDateTimeFormat, sqlDateFormat, sqlTimeFormat
  ) where
import Database.Selda.Backend.Internal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef

-- | Close a reusable Selda connection.
--   Closing a connection while in use is undefined.
--   Passing a closed connection to 'runSeldaT' results in a 'SeldaError'
--   being thrown. Closing a connection more than once is a no-op.
seldaClose :: MonadIO m => SeldaConnection -> m ()
seldaClose c = liftIO $ mask_ $ do
  closed <- atomicModifyIORef' (connClosed c) $ \closed -> (True, closed)
  unless closed $ closeConnection (connBackend c) c
