{-# LANGUAGE ExistentialQuantification #-}
-- | Test copilot-language:Copilot.Language.Reify.
--
-- The gist of this evaluation is in 'SemanticsP' and 'checkSemanticsP' which
-- evaluates an expression using Copilot's evaluator and compares it against
-- its expected meaning.
module Test.Copilot.Language.Reify where

-- External imports
import Data.Maybe                           (fromMaybe)
import Data.Typeable                        (Typeable)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Arbitrary, Gen, Property,
                                             arbitrary, chooseInt, elements,
                                             forAll, forAllShow, frequency,
                                             oneof)
import Test.QuickCheck.Monadic              (monadicIO, run)

-- Internal imports: library modules being tested
import           Copilot.Language                    (Typed)
import qualified Copilot.Language.Operators.Boolean  as Copilot
import qualified Copilot.Language.Operators.Constant as Copilot
import qualified Copilot.Language.Operators.Eq       as Copilot
import qualified Copilot.Language.Operators.Mux      as Copilot
import           Copilot.Language.Reify              (reify)
import           Copilot.Language.Spec               (observer)
import           Copilot.Language.Stream             (Stream)

-- Internal imports: functions needed to test after reification
import Copilot.Core.Type.Show      (ShowType (Haskell))
import Copilot.Core.Interpret.Eval (ExecTrace (interpObservers), eval)

-- Internal imports: auxiliary functions
import Test.Extra (apply1, apply2, apply3)

-- * Constants

-- | Max length of the traces being tested.
maxTraceLength :: Int
maxTraceLength = 200

-- | All unit tests for copilot-language:Copilot.Language.Reify.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Language.Reify"
    [ testProperty "eval Stream" testEvalExpr ]

-- * Individual tests

-- | Test for expression evaluation.
testEvalExpr :: Property
testEvalExpr =
  forAll (chooseInt (0, maxTraceLength)) $ \steps ->
  forAllShow arbitrarySemanticsP (semanticsShowK steps) $ \pair ->
  monadicIO $ run (checkSemanticsP steps [] pair)

-- * Random generators

-- ** Random SemanticsP generators

-- | An arbitrary expression, paired with its expected meaning.
--
-- See the function 'checkSemanticsP' to evaluate the pair.
arbitrarySemanticsP :: Gen SemanticsP
arbitrarySemanticsP = oneof
  [ SemanticsP <$> (arbitraryBoolExpr         :: Gen (Semantics Bool))
  ]

-- | An arbitrary boolean expression, paired with its expected meaning.
arbitraryBoolExpr :: Gen (Stream Bool, [Bool])
arbitraryBoolExpr =
  -- We use frequency instead of oneof because the random expression generator
  -- seems to generate expressions that are too large and the test fails due
  -- to running out of memory.
  frequency
    [ (10, arbitraryConst)

    , (5, arbitraryBoolOp0)

    , (5, apply1 <$> arbitraryBoolOp1 <*> arbitraryBoolExpr)

    , (1, apply2 <$> arbitraryBoolOp2
                 <*> arbitraryBoolExpr
                 <*> arbitraryBoolExpr)

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBoolExpr
                 <*> arbitraryBoolExpr)

    , (1, apply3 <$> arbitraryITEOp3
                 <*> arbitraryBoolExpr
                 <*> arbitraryBoolExpr
                 <*> arbitraryBoolExpr)
    ]

-- *** Primitives

-- | An arbitrary constant expression of any type, paired with its expected
-- meaning.
arbitraryConst :: (Arbitrary t, Typed t)
               => Gen (Stream t, [t])
arbitraryConst = (\v -> (Copilot.constant v, repeat v)) <$> arbitrary

-- | Generator for constant boolean streams, paired with their expected
-- meaning.
arbitraryBoolOp0 :: Gen (Stream Bool, [Bool])
arbitraryBoolOp0 = elements
  [ (Copilot.false, repeat False)
  , (Copilot.true,  repeat True)
  ]

-- *** Op 1

-- | Generator for arbitrary boolean operators with arity 1, paired with their
-- expected meaning.
arbitraryBoolOp1 :: Gen (Stream Bool -> Stream Bool, [Bool] -> [Bool])
arbitraryBoolOp1 = elements
  [ (Copilot.not, fmap not)
  ]

-- *** Op 2

-- | Generator for arbitrary boolean operators with arity 2, paired with their
-- expected meaning.
arbitraryBoolOp2 :: Gen ( Stream Bool -> Stream Bool -> Stream Bool
                        , [Bool] -> [Bool] -> [Bool]
                        )
arbitraryBoolOp2 = elements
  [ ((Copilot.&&),  zipWith (&&))
  , ((Copilot.||),  zipWith (||))
  , ((Copilot.==>), zipWith (\x y -> not x || y))
  , ((Copilot.xor), zipWith (\x y -> (x || y) && not (x && y)))
  ]

-- | Generator for arbitrary equality operators with arity 2, paired with their
-- expected meaning.
arbitraryEqOp2 :: (Typed t, Eq t)
               => Gen ( Stream t -> Stream t -> Stream Bool
                      , [t] -> [t] -> [Bool]
                      )
arbitraryEqOp2 = elements
  [ ((Copilot.==), zipWith (==))
  , ((Copilot./=), zipWith (/=))
  ]

-- *** Op 3

-- | Generator for if-then-else operator (with arity 3), paired with its
-- expected meaning.
--
-- Although this is constant and there is nothing arbitrary, we use the same
-- structure and naming convention as with others for simplicity.
arbitraryITEOp3 :: (Arbitrary t, Typed t)
                => Gen ( Stream Bool -> Stream t -> Stream t -> Stream t
                       , [Bool] -> [t] -> [t] -> [t]
                       )
arbitraryITEOp3 = return
  (Copilot.mux, zipWith3 (\x y z -> if x then y else z))

-- * Semantics

-- | Type that pairs an expression with its meaning as an infinite stream.
type Semantics t = (Stream t, [t])

-- | A phantom semantics pair is an existential type that encloses an
-- expression and its expected meaning as an infinite list of values.
--
-- It is needed by the arbitrary expression generator, to create a
-- heterogeneous list.
data SemanticsP = forall t
                . (Typeable t, Read t, Eq t, Show t, Typed t, Arbitrary t)
                => SemanticsP
  { semanticsPair :: (Stream t, [t])
  }

-- | Show function for test triplets that limits the accompanying list
-- to a certain length.
semanticsShowK :: Int -> SemanticsP -> String
semanticsShowK steps (SemanticsP (expr, exprList)) =
  show ("Cannot show stream", take steps exprList)

-- | Check that the expression in the semantics pair is evaluated to the given
-- list, up to a number of steps.
--
-- Some operations will overflow and return NaN. Because comparing any NaN
-- will, as per IEEE 754, always fail (i.e., return False), we handle that
-- specific case by stating that the test succeeds if any expected values
-- is NaN.
checkSemanticsP :: Int -> [a] -> SemanticsP -> IO Bool
checkSemanticsP steps _streams (SemanticsP (expr, exprList)) = do
    -- Spec with just one observer of one expression.
    let spec = observer testObserverName expr

    -- Reified stream (low-level)
    llSpec <- reify spec

    let trace = eval Haskell steps llSpec

    -- Limit expectation to the number of evaluation steps.
    let expectation = take steps exprList

    -- Obtain the results by looking up the observer in the spec
    -- and parsing the results into Haskell values.
    let resultValues = fmap readResult results
        results      = lookupWithDefault testObserverName []
                     $ interpObservers trace

    return $ any isNaN' expectation || resultValues == expectation

  where

    -- Fixed name for the observer. Used to obtain the result from the
    -- trace. It should be the only observer in the trace.
    testObserverName :: String
    testObserverName = "res"

    -- | Is NaN with Eq requirement only.
    isNaN' :: Eq a => a -> Bool
    isNaN' x = x /= x

-- * Auxiliary

-- | Read a Haskell value from the output of the evaluator.
readResult :: Read a => String -> a
readResult = read . readResult'
  where
    readResult' :: String -> String
    readResult' "false" = "False"
    readResult' "true"  = "True"
    readResult' s       = s

-- | Variant of 'lookup' with an additional default value returned when the key
-- provided is not found in the map.
lookupWithDefault :: Ord k => k -> v -> [(k, v)] -> v
lookupWithDefault k def = fromMaybe def . lookup k
