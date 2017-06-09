{-# LANGUAGE TypeOperators, OverloadedStrings, CPP, DeriveGeneric #-}
module Main where
import Control.Monad (when)
import System.Directory (doesFileExist, removeFile, getTemporaryDirectory)
import System.Exit (exitFailure)
import Test.HUnit
import Test.HUnit.Text
import Database.Selda (SeldaM, setLocalCache)
import Tests.Query (queryTests)
import Tests.Mutable (mutableTests, invalidateCacheAfterTransaction)
import Tests.Validation (validationTests)
import Tests.NonDB (noDBTests)
import Tests.MultiConn (multiConnTests)
import Tables (teardown)

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
freshEnv :: FilePath -> SeldaM a -> IO a
#ifdef POSTGRES
freshEnv _ m = withPostgreSQL pgConnectInfo $ teardown >> m
#else
freshEnv file m = do
  exists <- doesFileExist file
  when exists $ removeFile file
  x <- withSQLite file m
  removeFile file
  return x
#endif

allTests f = TestList
  [ "non-database tests"       ~: noDBTests
  , "query tests"              ~: queryTests run
  , "validation tests"         ~: validationTests (freshEnv f)
  , "mutable tests"            ~: mutableTests (freshEnv f)
  , "mutable tests (caching)"  ~: mutableTests caching
  , "cache + transaction race" ~: invalidateCacheAfterTransaction run
  , "multi-connection tests"   ~: multiConnTests open
  ]
  where
    caching m = freshEnv f (setLocalCache 1000 >> m)
#ifdef POSTGRES
    open = pgOpen pgConnectInfo
    run = withPostgreSQL pgConnectInfo
#else
    open = sqliteOpen f
    run = withSQLite f
#endif
