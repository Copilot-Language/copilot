-- | Test copilot-language.
module Main where

-- External imports
import Test.Framework (Test, defaultMain)

-- Internal imports
import qualified Test.Copilot.Language.Reify

-- | Run all unit tests on copilot-language.
main :: IO ()
main = defaultMain tests

-- | All unit tests in copilot-language.
tests :: [Test.Framework.Test]
tests =
  [ Test.Copilot.Language.Reify.tests
  ]
