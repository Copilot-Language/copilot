-- | Test copilot-core.
module Main where

-- External imports
import Test.Framework (Test, defaultMain)

-- Internal library modules being tested
import qualified Test.Copilot.Core.Type
import qualified Test.Copilot.Core.Type.Array

-- | Run all unit tests on copilot-core.
main :: IO ()
main = defaultMain tests

-- | All unit tests in copilot-core.
tests :: [Test.Framework.Test]
tests =
  [ Test.Copilot.Core.Type.tests
  , Test.Copilot.Core.Type.Array.tests
  ]
