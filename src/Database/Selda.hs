-- | Selda is not LINQ, but they're definitely related.
--
--   Selda is a high-level EDSL for interacting with relational databases.
module Database.Selda
  ( -- * Running queries
    SeldaT, Table, Query, Col, MonadIO (..), MonadTrans (..)
  , query
    -- * Constructing queries
  , (:*:)(..)
  , select, restrict
  , (.==), (./=), (.>), (.<), (.>=), (.<=), like
  , not_, literal
    -- * Constructing tables
  , TableName, ColName, Proxy (..)
  , table, (¤), primary, required, nullable
  ) where
import Database.Selda.Table
import Database.Selda.Column
import Database.Selda.Query
import Database.Selda.Backend
import Data.Proxy

(.==), (./=), (.>), (.<), (.>=), (.<=) :: Col a -> Col a -> Col Bool
(.==) = BinOp Eq
a ./= b = not_ (a .== b)
(.>)  = BinOp Gt
(.<)  = BinOp Lt
(.>=) = BinOp Gte
(.<=) = BinOp Lte
infixl 4 .==
infixl 4 .>
infixl 4 .<
infixl 4 .>=
infixl 4 .<=

like :: Col String -> Col String -> Col Bool
like = BinOp Like
infixl 4 `like`

not_ :: Col Bool -> Col Bool
not_ = UnOp Not
