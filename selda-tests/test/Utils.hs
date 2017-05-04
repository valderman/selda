-- | Utility functions that are useful for all tests.
module Utils where
import Control.Monad.Catch
import Database.Selda
import Test.HUnit

-- | Assert that the given computation should fail.
assertFail :: SeldaT IO a -> SeldaT IO ()
assertFail m = do
  res <- try m
  case res of
    Left (SomeException _) -> return ()
    _                      -> liftIO $ assertFailure "computation did not fail"

-- | @SeldaT@ wrapper for 'assertEqual'.
assEq :: (Show a, Eq a) => String -> a -> a -> SeldaT IO ()
assEq s expect actual = liftIO $ assertEqual s expect actual

-- | @SeldaT@ wrapper for 'assertBool'.
ass :: String -> Bool -> SeldaT IO ()
ass s pred = liftIO $ assertBool s pred
