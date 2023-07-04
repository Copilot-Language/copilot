-- | Test copilot-c99.
module Main where

-- External imports
import Test.Framework (Test, defaultMain)

-- Internal library modules being tested
import qualified Test.Copilot.Compile.C99

-- | Run all unit tests on copilot-c99.
main :: IO ()
main = defaultMain tests

-- | All unit tests in copilot-c99.
tests :: [Test.Framework.Test]
tests =
  [ Test.Copilot.Compile.C99.tests
  ]
