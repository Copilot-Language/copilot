-- | Test copilot-core:Copilot.Core.Error.
module Test.Copilot.Core.Error where

-- External imports
import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (assertBool)

-- Internal imports: auxiliary testing facilities
import Test.Extra (withoutException)

-- Internal imports: library modules being tested
import Copilot.Core.Error (badUsage, impossible)

-- | All unit tests for copilot-core:Copilot.Core.Error.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Core.Error"
    [ testCase "impossible" testErrorImpossible
    , testCase "badUsage"   testErrorBadUsage
    ]

-- * Individual tests

-- | Test that 'Error.impossible' always throws an exception of some kind.
testErrorImpossible :: IO ()
testErrorImpossible = do
  res <- not <$> withoutException (impossible "func1" "pkg1")
  assertBool "The function impossible should fail with an exception" res

-- | Test that 'Error.badUsage' always throws an exception of some kind.
testErrorBadUsage :: IO ()
testErrorBadUsage = do
  res <- not <$> withoutException (badUsage "func1")
  assertBool "The function badUsage should fail with an exception" res
