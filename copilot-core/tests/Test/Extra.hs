{-# LANGUAGE ScopedTypeVariables #-}
-- | Auxiliary testing helper functions.
module Test.Extra where

-- External imports
import Control.Exception (SomeException, catch)

-- | Test that a computation terminates without throwing an exception.
--
-- Returns 'True' if the computation can be completed without an exception,
-- and 'False' otherwise.
withoutException :: IO a -> IO Bool
withoutException e =
  catch (e >> return True)
        (\(_ :: SomeException) -> return False)
