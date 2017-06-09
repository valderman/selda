{-# LANGUAGE OverloadedStrings #-}
-- | Tests that need to open and close different connections to the database.
module Tests.MultiConn (multiConnTests) where
import Control.Monad.Catch
import Database.Selda
import Database.Selda.Backend
import Test.HUnit
import Utils
import Tables

multiConnTests :: IO SeldaConnection -> Test
multiConnTests open = test
  [ "setup with runSeldaT" ~: open >>= runSeldaT (teardown >> setup)
  , "connection unusable post-close" ~: postClose open
  , "connection is reusable" ~: reuse open
  , "simultaneous connections" ~: simultaneousConnections open
  , "simultaneous writes" ~: simultaneousConnections open
  , "teardown with runSeldaT" ~: open >>= runSeldaT teardown
  ]

postClose :: IO SeldaConnection -> IO ()
postClose open = do
  conn <- open
  seldaClose conn
  res <- try $ flip runSeldaT conn $ do
    query $ select people
  case res of
    Left (DbError{}) -> return ()
    _                -> liftIO $ assertFailure "post-close error not thrown"

reuse :: IO SeldaConnection -> IO ()
reuse open = do
  conn <- open
  res1 <- flip runSeldaT conn $ do
    query $ (pName `from` select people) `suchThat` (.== "Link")
  assertEqual "wrong result from first query" ["Link"] res1

  res2 <- flip runSeldaT conn $ do
    query $ (pName `from` select people) `suchThat` (.== "Kobayashi")
  assertEqual "wrong result from second query" ["Kobayashi"] res2
  seldaClose conn

simultaneousConnections :: IO SeldaConnection -> IO ()
simultaneousConnections open = do
  c1 <- open
  c2 <- open
  res1 <- flip runSeldaT c1 $ do
    query $ (pName `from` select people) `suchThat` (.== "Link")
  assertEqual "wrong result from first query" ["Link"] res1

  res2 <- flip runSeldaT c2 $ do
    query $ (pName `from` select people) `suchThat` (.== "Kobayashi")
  assertEqual "wrong result from second query" ["Kobayashi"] res2
  seldaClose c1
  seldaClose c2

simultaneousWrites :: IO SeldaConnection -> IO ()
simultaneousWrites open = do
    c1 <- open
    c2 <- open
    withC c1 c2 $ runSeldaT (teardown >> setup)
    flip runSeldaT c1 $ do
      insert_ people ["Marina" :*: 18 :*: def :*: def]
    flip runSeldaT c2 $ do
      insert_ people ["Amber" :*: 19 :*: def :*: def]
    res1 <- flip runSeldaT c1 $ do
      query $ (pName `from` select people) `suchThat` (.== "Amber")
    res2 <- flip runSeldaT c1 $ do
      query $ (pName `from` select people) `suchThat` (.== "Marina")
    withC c1 c2 $ runSeldaT teardown
    withC c1 c2 $ seldaClose
    assertEqual "wrong result from first query" ["Amber"] res1
    assertEqual "wrong result from second query" ["Marina"] res2
  where
    withC c1 c2 f = f c1 >> f c2
