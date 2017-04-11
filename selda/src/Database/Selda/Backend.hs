{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | API for building Selda backends.
module Database.Selda.Backend
  ( MonadIO (..)
  , QueryRunner, SeldaBackend (..), MonadSelda (..), SeldaT (..)
  , Param (..), Lit (..), SqlValue (..), ColAttr (..)
  , compileColAttr
  , runSeldaT
  ) where
import Database.Selda.SQL (Param (..))
import Database.Selda.SqlType
import Database.Selda.Table (ColAttr (..))
import Database.Selda.Table.Compile (compileColAttr)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Text (Text)

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
  }

-- | Some monad with Selda SQL capabilitites.
class MonadIO m => MonadSelda m where
  seldaBackend :: m SeldaBackend

-- | Monad transformer adding Selda SQL capabilities.
newtype SeldaT m a = S {unS :: ReaderT SeldaBackend m a}
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask, MonadTrans
           )

instance MonadIO m => MonadSelda (SeldaT m) where
  seldaBackend = S ask

-- | Run a Selda transformer. Backends should use this to implement their
--   @withX@ functions.
runSeldaT :: SeldaT m a -> SeldaBackend -> m a
runSeldaT m = runReaderT (unS m)
