{-# LANGUAGE OverloadedStrings #-}
-- | Tests that need to open and close different connections to the database.
module Tests.MultiConn (multiConnTests) where
import Database.Selda
import Database.Selda.Backend
import Control.Concurrent
import Control.Monad.Catch
import Data.IORef
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
  , "connection access is serialized" ~: serialized open
  , "unrelated connections are not serialized" ~: twoConnsNotSerialized open
  , "connection reuse after exception" ~: reuseAfterException open
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

serialized :: IO SeldaConnection -> IO ()
serialized open = do
  conn <- open
  ref <- newIORef 0

  forkIO $ do
    flip runSeldaT conn $ do
      liftIO $ atomicModifyIORef' ref $ \r -> (r+1, ())
      liftIO $ threadDelay 200000
      liftIO $ atomicModifyIORef' ref $ \r -> (r-1, ())

  threadDelay 100000
  res <- flip runSeldaT conn $ liftIO $ readIORef ref
  seldaClose conn
  assertEqual "concurrent use of the same connection" 0 res

twoConnsNotSerialized :: IO SeldaConnection -> IO ()
twoConnsNotSerialized open = do
  c1 <- open
  c2 <- open
  ref <- newIORef 0

  forkIO $ do
    flip runSeldaT c1 $ do
      liftIO $ atomicModifyIORef' ref $ \r -> (r+1, ())
      liftIO $ threadDelay 200000
      liftIO $ atomicModifyIORef' ref $ \r -> (r-1, ())

  threadDelay 100000
  res <- flip runSeldaT c2 $ liftIO $ readIORef ref
  seldaClose c1
  seldaClose c2
  assertEqual "unrelated connections were serialized" 1 res

reuseAfterException :: IO SeldaConnection -> IO ()
reuseAfterException open = do
  conn <- open
  res <- try $ flip runSeldaT conn $ do
    error "oh noes!"
  case res of
    Right _ -> do
      assertFailure "computation didn't fail"
    Left (SomeException{}) -> do
      res' <- flip runSeldaT conn $ do
        query $ (pName `from` select people) `suchThat` (.== "Miyu")
      assertEqual "got wrong result after exception" ["Miyu"] res'
