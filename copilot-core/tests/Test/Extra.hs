{-# LANGUAGE ScopedTypeVariables #-}
-- | Auxiliary testing helper functions.
module Test.Extra where

-- External imports
import Control.Arrow     ((***))
import Control.Exception (SomeException, catch)

-- * Detecting exceptions

-- | Test that a computation terminates without throwing an exception.
--
-- Returns 'True' if the computation can be completed without an exception,
-- and 'False' otherwise.
withoutException :: IO a -> IO Bool
withoutException e =
  catch (e >> return True)
        (\(_ :: SomeException) -> return False)

-- * Function application

-- | Apply a tuple with two functions to a tuple of arguments.
apply1 :: (a1 -> b1, a2 -> b2) -- ^ Pair with functions
       -> (a1, a2)             -- ^ Pair with arguments
       -> (b1, b2)             -- ^ Pair with results
apply1 = uncurry (***)

-- | Apply a tuple with two functions on two arguments to their tupled
-- arguments.
apply2 :: (a1 -> b1 -> c1, a2 -> b2 -> c2) -- ^ Pair with functions
       -> (a1, a2)                         -- ^ Pair with first arguments
       -> (b1, b2)                         -- ^ Pair with second arguments
       -> (c1, c2)                         -- ^ Pair with results
apply2 fs = apply1 . apply1 fs

-- | Apply a tuple with two functions on three arguments to their tupled
-- arguments.
apply3 :: (a1 -> b1 -> c1 -> d1, a2 -> b2 -> c2 -> d2)
                    -- ^ Pair with functions
       -> (a1, a2)  -- ^ Pair with first arguments
       -> (b1, b2)  -- ^ Pair with second arguments
       -> (c1, c2)  -- ^ Pair with third arguments
       -> (d1, d2)  -- ^ Pair with results
apply3 fs = apply2 . apply1 fs
