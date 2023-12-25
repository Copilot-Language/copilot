-- | Test copilot-libraries:Copilot.Library.PTLTL
module Test.Copilot.Library.PTLTL
    (tests)
  where

-- External imports
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Gen, Property)

-- External imports: Copilot
import           Copilot.Language                   (extern)
import qualified Copilot.Language.Operators.Boolean as Copilot
import           Copilot.Language.Stream            (Stream)
import           Copilot.Theorem.What4              (SatResult (..))

-- Internal imports: auxiliary functions
import Test.Extra (arbitraryBoolExpr, testWithInterpreter, testWithTheorem)

-- Internal imports: Modules being tested
import Copilot.Library.PTLTL (eventuallyPrev, previous)

-- * Constants

-- | Unit tests for copilot-libraries:Copilot.Library.PTLTL.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Library.PTLTL"
    [ testProperty "previous x ==> eventuallyPrev x (theorem)"
        testProvePreviousEventually
    , testProperty "previous x ==> eventuallyPrev x (interpreter)"
        testCheckPreviousEventually
    ]

-- * Individual tests

-- | Test that Z3 is able to prove the following expression valid:
-- @
--    previous x ==> eventuallyPrev x
-- @
testProvePreviousEventually :: Property
testProvePreviousEventually = testWithTheorem pair
  where
    pair :: Gen (Stream Bool, SatResult)
    pair = pure (stream, expectation)

    stream :: Stream Bool
    stream =
        previous boolStream Copilot.==> eventuallyPrev boolStream
      where
        boolStream = extern "x" Nothing

    expectation :: SatResult
    expectation = Valid

-- | Test that the following stream is always true:
-- @
--    previous x ==> eventuallyPrev x
-- @
testCheckPreviousEventually :: Property
testCheckPreviousEventually = testWithInterpreter pair
  where
    pair :: Gen (Stream Bool, [Bool])
    pair = do
      -- We discard the expectation from the expression; the temporal formula
      -- holds at all times regardless.
      boolStream <- fst <$> arbitraryBoolExpr
      let prop = previous boolStream Copilot.==> eventuallyPrev boolStream
      return (prop, expectation)

    expectation :: [Bool]
    expectation = repeat True
