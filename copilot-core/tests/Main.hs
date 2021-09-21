-- | Test copilot-core.
module Main where

import Data.Monoid                    ( mempty )
import Test.Framework                 ( Test, defaultMainWithOpts )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit                     ( assertBool )

-- | Run all unit tests on copilot-core.
main :: IO ()
main = defaultMainWithOpts tests mempty

-- | All unit tests in copilot-core.
tests :: [Test.Framework.Test]
tests =
  [ testCase "Success" succeedOnPurpose
  ]

-- | Dummy test case that should always pass.
succeedOnPurpose :: IO ()
succeedOnPurpose = do
    assertBool errorMsg True
  where
    errorMsg = "Unexpected item in the bagging area."
