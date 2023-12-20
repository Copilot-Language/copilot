-- | Test copilot-theorem.
module Main where

-- External imports
import Test.Framework (Test, defaultMain)

-- Internal imports
import qualified Test.Copilot.Theorem.What4

-- | Run all unit tests on copilot-theorem.
main :: IO ()
main = defaultMain tests

-- | All unit tests in copilot-theorem.
tests :: [Test.Framework.Test]
tests =
  [ Test.Copilot.Theorem.What4.tests
  ]
