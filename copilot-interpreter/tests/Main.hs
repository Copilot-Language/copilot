-- | Test copilot-core.
module Main where

-- External imports
import Test.Framework (Test, defaultMain)

-- Internal library modules being tested
import qualified Test.Copilot.Interpret.Eval

-- | Run all unit tests on copilot-core.
main :: IO ()
main = defaultMain tests

-- | All unit tests in copilot-core.
tests :: [Test.Framework.Test]
tests =
  [ Test.Copilot.Interpret.Eval.tests
  ]
