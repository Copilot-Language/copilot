module Main (main) where

import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.IO (stderr, stdout)
import System.IO.Silently (hSilence)
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

import Copilot.Verifier (Verbosity(..))
import Copilot.Verifier.Examples (shouldFailExamples, shouldPassExamples)

main :: IO ()
main = defaultMain $
  testGroup "copilot-verifier-examples tests"
    [ testGroup "should-fail tests" $
        -- Why use hSilence for the should-fail tests when we are passing
        -- Quiet? It's because crux-llvm errors are logged at the highest
        -- severity possible, and even Crux's quietMode isn't enough to
        -- suppress those messages. We could try messing with things on the
        -- Crux side to avoid this, but it's simpler just to use hSilence here.
        -- After all, we don't really care about the output of failing tests
        -- anyway, just their exit codes.
        map (\(name, action) -> expectFail (testCase (Text.unpack (CI.original name))
                                                     (hSilence [stderr, stdout] action)))
            (Map.toAscList (shouldFailExamples Quiet))
    , testGroup "should-pass tests" $
        map (\(name, action) -> testCase (Text.unpack (CI.original name)) action)
            (Map.toAscList (shouldPassExamples Quiet))
    ]
