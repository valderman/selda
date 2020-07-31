{-# LANGUAGE TypeOperators, OverloadedStrings, CPP, DeriveGeneric #-}
module Main where
import Control.Monad (when)
import System.Directory (doesFileExist, removeFile, getTemporaryDirectory)
import System.Exit (exitFailure)
import Test.HUnit
import Test.HUnit.Text
import Database.Selda (SeldaM)
import Tests.Query (queryTests)
import Tests.Mutable (mutableTests)
import Tests.Validation (validationTests)
import Tests.NonDB (noDBTests)
import Tests.MultiConn (multiConnTests)
import Tests.PGConnectionString (pgConnectionStringTests)
import Tables (teardown)

#ifdef TEST_JSON
import Tests.JSON (jsonQueryTests)
#endif
import Tests.JSON (jsonTests)

#ifdef POSTGRES
-- To test the PostgreSQL backend, specify the connection info for the server
-- as PGConnectInfo.pgConnectInfo :: PGConnectInfo.
import Database.Selda.PostgreSQL
import PGConnectInfo (pgConnectInfo)
#else
import Database.Selda.SQLite
#endif

main = do
  tmpdir <- getTemporaryDirectory
  let dbfile = tmpdir ++ "/" ++ "__selda_test_tmp.sqlite"
  freshEnv dbfile $ teardown
  result <- runTestTT (allTests dbfile)
  case result of
    Counts cs tries 0 0 -> return ()
    _                   -> exitFailure

-- | Run the given computation over the given SQLite file. If the file exists,
--   it will be removed first.
#ifdef POSTGRES
freshEnv :: FilePath -> SeldaM PG a -> IO a
freshEnv _ m = withPostgreSQL pgConnectInfo $ teardown >> m
#else
freshEnv :: FilePath -> SeldaM SQLite a -> IO a
freshEnv file m = do
  exists <- doesFileExist file
  when exists $ removeFile file
  x <- withSQLite file m
  removeFile file
  return x
#endif

allTests f = TestList
  [ "non-database tests"     ~: noDBTests
  , "query tests"            ~: queryTests run
  , "validation tests"       ~: validationTests (freshEnv f)
  , "mutable tests"          ~: mutableTests (freshEnv f)
  , "multi-connection tests" ~: multiConnTests open
  , "mandatory json tests"   ~: jsonTests (freshEnv f)
#ifdef TEST_JSON
  , "json query tests"       ~: jsonQueryTests (freshEnv f)
#endif
#ifdef POSTGRES
  , "pg connection string"   ~: pgConnectionStringTests pgConnectInfo
#endif
  ]
  where
#ifdef POSTGRES
    open = pgOpen pgConnectInfo
    run = withPostgreSQL pgConnectInfo
#else
    open = sqliteOpen f
    run = withSQLite f
#endif
