-- | Test the Kind2 backend of copilot-theorem.
--
-- These tests require the @kind2@ executable (version 1.0 or newer) to be in
-- the @PATH@.
module Main where

-- External imports
import Test.Framework (Test, defaultMain)

-- Internal imports
import qualified Test.Copilot.Theorem.Kind2

-- | Run all unit tests on the Kind2 backend of copilot-theorem.
main :: IO ()
main = defaultMain tests

-- | All unit tests on the Kind2 backend of copilot-theorem.
tests :: [Test.Framework.Test]
tests =
  [ Test.Copilot.Theorem.Kind2.tests
  ]
