-- | Test copilot-libraries.
module Main where

-- External imports
import Test.Framework (Test, defaultMain)

-- Internal imports
import qualified Test.Copilot.Library.PTLTL

-- | Run all unit tests on copilot-libraries.
main :: IO ()
main = defaultMain tests

-- | All unit tests in copilot-libraries.
tests :: [Test.Framework.Test]
tests =
  [ Test.Copilot.Library.PTLTL.tests
  ]
