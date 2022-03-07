-- | Test copilot-language.
module Main where

-- External imports
import Data.Monoid    (mempty)
import Test.Framework (Test, defaultMainWithOpts)

-- | Run all unit tests on copilot-language.
main :: IO ()
main = defaultMainWithOpts tests mempty

-- | All unit tests in copilot-language.
tests :: [Test.Framework.Test]
tests = []
