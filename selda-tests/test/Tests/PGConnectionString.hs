module Tests.PGConnectionString (pgConnectionStringTests) where
import Data.Text.Encoding (decodeUtf8)
import Database.Selda
import Database.Selda.PostgreSQL
import Test.HUnit
import Tables

pgConnectionStringTests :: PGConnectInfo -> Test
pgConnectionStringTests s@(PGConnectionString _ _) =
  test [ "setup" ~: withPostgreSQL s (teardown >> setup :: SeldaM PG ()) ]
pgConnectionStringTests ci =
  pgConnectionStringTests (PGConnectionString (decodeUtf8 $ pgConnString ci) Nothing)
