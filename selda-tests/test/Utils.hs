-- | Utility functions that are useful for all tests.
module Utils where
import Control.Monad.Catch
import Database.Selda
import Test.HUnit

-- | Assert that the given computation should fail.
assertFail :: SeldaM b a -> SeldaM b ()
assertFail m = do
  res <- try m
  case res of
    Left (SomeException _) -> return ()
    _                      -> liftIO $ assertFailure "computation did not fail"

-- | @SeldaT@ wrapper for 'assertEqual'.
assEq :: (Show a, Eq a) => String -> a -> a -> SeldaM b ()
assEq s expect actual = liftIO $ assertEqual s expect actual

-- | @SeldaT@ wrapper for 'assertBool'.
ass :: String -> Bool -> SeldaM b ()
ass s pred = liftIO $ assertBool s pred
