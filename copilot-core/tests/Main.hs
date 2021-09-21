-- | Test copilot-core.
module Main where

-- External imports
import Data.Monoid    (mempty)
import Test.Framework (Test, defaultMainWithOpts)

-- Internal library modules being tested
import qualified Test.Copilot.Core.Error
import qualified Test.Copilot.Core.External

-- | Run all unit tests on copilot-core.
main :: IO ()
main = defaultMainWithOpts tests mempty

-- | All unit tests in copilot-core.
tests :: [Test.Framework.Test]
tests =
  [ Test.Copilot.Core.Error.tests
  , Test.Copilot.Core.External.tests
  ]
