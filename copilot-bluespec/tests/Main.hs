-- | Test copilot-bluespec.
module Main where

-- External imports
import Test.Framework (Test, defaultMain)

-- Internal library modules being tested
import qualified Test.Copilot.Compile.Bluespec

-- | Run all @copilot-bluespec@ tests.
main :: IO ()
main = defaultMain tests

-- | All @copilot-bluespec@ tests.
tests :: [Test.Framework.Test]
tests =
  [ Test.Copilot.Compile.Bluespec.tests
  ]
