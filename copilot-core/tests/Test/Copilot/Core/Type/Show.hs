-- The following warning is disnabled in this module so that the import of
-- Copilot.Core.Type.Show does not give rise to a warning.
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | Test copilot-core:Copilot.Core.Type.Show.
module Test.Copilot.Core.Type.Show where

-- External imports
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Arbitrary, Property, property)
import Text.Read                            (readMaybe)

-- Internal imports: library modules being tested
import Copilot.Core.Type      (Type (Double, Float, Int16, Int32, Int64))
import Copilot.Core.Type.Show (ShowType (Haskell), showWithType)

-- | All unit tests for copilot-core:Copilot.Core.Type.Show.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Core.Type.Show"
    [ testProperty "read . showWithType == identity (Int16)"
        (testShowRead Int16)
    , testProperty "read . showWithType == identity (Int32)"
        (testShowRead Int32)
    , testProperty "read . showWithType == identity (Int64)"
        (testShowRead Int64)
    , testProperty "read . showWithType == identity (Float)"
        (testShowRead Float)
    , testProperty "read . showWithType == identity (Double)"
        (testShowRead Double)
    ]

-- | Test that showing a value with 'showWithType' and reading it back results
-- in the same value.
testShowRead :: (Arbitrary a, Eq a, Read a) => Type a -> a -> Property
testShowRead t v = property $
  Just v == readMaybe (showWithType Haskell t v)
