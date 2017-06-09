-- | API for building Selda backends and adding support for more types
--   in queries.
module Database.Selda.Backend
  ( MonadSelda (..), SeldaT, SeldaM, SeldaError (..)
  , StmtID, QueryRunner, SeldaBackend (..), SeldaConnection
  , SqlType (..), SqlValue (..), SqlTypeRep (..)
  , Param (..), Lit (..), ColAttr (..)
  , PPConfig (..), defPPConfig
  , newConnection, allStmts
  , runSeldaT, seldaBackend
  , sqlDateTimeFormat, sqlDateFormat, sqlTimeFormat
  ) where
import Database.Selda.Backend.Internal
