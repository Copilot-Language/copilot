{-# LANGUAGE ExistentialQuantification #-}
-- | Test copilot-language:Copilot.Language.Reify.
--
-- The gist of this evaluation is in 'SemanticsP' and 'checkSemanticsP' which
-- evaluates an expression using Copilot's evaluator and compares it against
-- its expected meaning.
module Test.Copilot.Language.Reify where

-- External imports
import Data.Bits                            (Bits, complement, shiftL, shiftR,
                                             xor, (.&.), (.|.))
import Data.Int                             (Int16, Int32, Int64, Int8)
import Data.Maybe                           (fromMaybe)
import Data.Typeable                        (Typeable)
import Data.Word                            (Word16, Word32, Word64, Word8)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Arbitrary, Gen, Property,
                                             arbitrary, chooseInt, elements,
                                             forAll, forAllShow, frequency,
                                             oneof)
import Test.QuickCheck.Monadic              (monadicIO, run)

-- Internal imports: library modules being tested
import           Copilot.Language                    (Typed)
import qualified Copilot.Language.Operators.BitWise  as Copilot
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
  , SemanticsP <$> (arbitraryNumExpr          :: Gen (Semantics Int8))
  , SemanticsP <$> (arbitraryNumExpr          :: Gen (Semantics Int16))
  , SemanticsP <$> (arbitraryNumExpr          :: Gen (Semantics Int32))
  , SemanticsP <$> (arbitraryNumExpr          :: Gen (Semantics Int64))
  , SemanticsP <$> (arbitraryNumExpr          :: Gen (Semantics Word8))
  , SemanticsP <$> (arbitraryNumExpr          :: Gen (Semantics Word16))
  , SemanticsP <$> (arbitraryNumExpr          :: Gen (Semantics Word32))
  , SemanticsP <$> (arbitraryNumExpr          :: Gen (Semantics Word64))
  , SemanticsP <$> (arbitraryBitsExpr         :: Gen (Semantics Bool))
  , SemanticsP <$> (arbitraryBitsExpr         :: Gen (Semantics Int8))
  , SemanticsP <$> (arbitraryBitsExpr         :: Gen (Semantics Int16))
  , SemanticsP <$> (arbitraryBitsExpr         :: Gen (Semantics Int32))
  , SemanticsP <$> (arbitraryBitsExpr         :: Gen (Semantics Int64))
  , SemanticsP <$> (arbitraryBitsExpr         :: Gen (Semantics Word8))
  , SemanticsP <$> (arbitraryBitsExpr         :: Gen (Semantics Word16))
  , SemanticsP <$> (arbitraryBitsExpr         :: Gen (Semantics Word32))
  , SemanticsP <$> (arbitraryBitsExpr         :: Gen (Semantics Word64))
  , SemanticsP <$> (arbitraryBitsIntegralExpr :: Gen (Semantics Int8))
  , SemanticsP <$> (arbitraryBitsIntegralExpr :: Gen (Semantics Int16))
  , SemanticsP <$> (arbitraryBitsIntegralExpr :: Gen (Semantics Int32))
  , SemanticsP <$> (arbitraryBitsIntegralExpr :: Gen (Semantics Int64))
  , SemanticsP <$> (arbitraryBitsIntegralExpr :: Gen (Semantics Word8))
  , SemanticsP <$> (arbitraryBitsIntegralExpr :: Gen (Semantics Word16))
  , SemanticsP <$> (arbitraryBitsIntegralExpr :: Gen (Semantics Word32))
  , SemanticsP <$> (arbitraryBitsIntegralExpr :: Gen (Semantics Word64))
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

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Stream Int8, [Int8])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Stream Int16, [Int16])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Stream Int32, [Int32])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Stream Int64, [Int64])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Stream Word8, [Word8])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Stream Word16, [Word16])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Stream Word32, [Word32])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Stream Word64, [Word64])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Int8, [Int8])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Int16, [Int16])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Int32, [Int32])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Int64, [Int64])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Word8, [Word8])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Word16, [Word16])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Word32, [Word32])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Word64, [Word64])))

    , (1, apply3 <$> arbitraryITEOp3
                 <*> arbitraryBoolExpr
                 <*> arbitraryBoolExpr
                 <*> arbitraryBoolExpr)
    ]

-- | An arbitrary numeric expression, paired with its expected meaning.
arbitraryNumExpr :: (Arbitrary t, Typed t, Num t, Eq t)
                 => Gen (Stream t, [t])
arbitraryNumExpr =
  -- We use frequency instead of oneof because the random expression generator
  -- seems to generate expressions that are too large and the test fails due
  -- to running out of memory.
  frequency
    [ (10, arbitraryConst)

    , (5, apply1 <$> arbitraryNumOp1 <*> arbitraryNumExpr)

    , (2, apply2 <$> arbitraryNumOp2 <*> arbitraryNumExpr <*> arbitraryNumExpr)

    , (2, apply3 <$> arbitraryITEOp3
                 <*> arbitraryBoolExpr
                 <*> arbitraryNumExpr
                 <*> arbitraryNumExpr)
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

-- | An arbitrary Bits expression, paired with its expected meaning.
arbitraryBitsExpr :: (Arbitrary t, Typed t, Bits t)
                  => Gen (Stream t, [t])
arbitraryBitsExpr =
  -- We use frequency instead of oneof because the random expression generator
  -- seems to generate expressions that are too large and the test fails due
  -- to running out of memory.
  frequency
    [ (10, arbitraryConst)

    , (5, apply1 <$> arbitraryBitsOp1 <*> arbitraryBitsExpr)

    , (2, apply2 <$> arbitraryBitsOp2
                 <*> arbitraryBitsExpr
                 <*> arbitraryBitsExpr)

    , (2, apply3 <$> arbitraryITEOp3
                 <*> arbitraryBoolExpr
                 <*> arbitraryBitsExpr <*> arbitraryBitsExpr)
    ]

-- | An arbitrary expression for types that are instances of Bits and Integral,
-- paired with its expected meaning.
arbitraryBitsIntegralExpr :: (Arbitrary t, Typed t, Bits t, Integral t)
                          => Gen (Stream t, [t])
arbitraryBitsIntegralExpr =
  -- We use frequency instead of oneof because the random expression generator
  -- seems to generate expressions that are too large and the test fails due
  -- to running out of memory.
  frequency
    [ (10, arbitraryConst)

    , (2, apply1 <$> arbitraryNumOp1 <*> arbitraryBitsIntegralExpr)

    , (1, apply2 <$> arbitraryNumOp2
                 <*> arbitraryBitsIntegralExpr
                 <*> arbitraryBitsIntegralExpr)

    , (5, apply2 <$> arbitraryBitsIntegralOp2
                 <*> arbitraryBitsIntegralExpr
                 <*> arbitraryBitsIntegralExpr)

    , (1, apply3 <$> arbitraryITEOp3
                 <*> arbitraryBoolExpr
                 <*> arbitraryBitsIntegralExpr
                 <*> arbitraryBitsIntegralExpr)
    ]


-- *** Op 1

-- | Generator for arbitrary boolean operators with arity 1, paired with their
-- expected meaning.
arbitraryBoolOp1 :: Gen (Stream Bool -> Stream Bool, [Bool] -> [Bool])
arbitraryBoolOp1 = elements
  [ (Copilot.not, fmap not)
  ]

-- | Generator for arbitrary numeric operators with arity 1, paired with their
-- expected meaning.
arbitraryNumOp1 :: (Typed t, Num t, Eq t)
                => Gen (Stream t -> Stream t, [t] -> [t])
arbitraryNumOp1 = elements
  [ (abs,    fmap abs)
  , (signum, fmap signum)
  ]

-- | Generator for arbitrary bitwise operators with arity 1, paired with their
-- expected meaning.
arbitraryBitsOp1 :: (Typed t, Bits t)
                 => Gen (Stream t -> Stream t, [t] -> [t])
arbitraryBitsOp1 = elements
  [ (complement, fmap complement)
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

-- | Generator for arbitrary numeric operators with arity 2, paired with their
-- expected meaning.
arbitraryNumOp2 :: (Typed t, Num t, Eq t)
                => Gen (Stream t -> Stream t -> Stream t, [t] -> [t] -> [t])
arbitraryNumOp2 = elements
  [ ((+), zipWith (+))
  , ((-), zipWith (-))
  , ((*), zipWith (*))
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

-- | Generator for arbitrary bitwise operators with arity 2, paired with their
-- expected meaning.
arbitraryBitsOp2 :: (Typed t, Bits t)
                 => Gen (Stream t -> Stream t -> Stream t, [t] -> [t] -> [t])
arbitraryBitsOp2 = elements
  [ ((.&.), zipWith (.&.))
  , ((.|.), zipWith (.|.))
  , (xor,   zipWith xor)
  ]

-- | Generator for arbitrary bit shifting operators with arity 2, paired with
-- their expected meaning.
--
-- This generator is a bit more strict in its type signature than the
-- underlying bit-shifting operators being tested, since it enforces both the
-- value being manipulated and the value that indicates how much to shift by to
-- have the same type.
arbitraryBitsIntegralOp2 :: (Typed t, Bits t, Integral t)
                         => Gen ( Stream t -> Stream t -> Stream t
                                , [t] -> [t] -> [t]
                                )
arbitraryBitsIntegralOp2 = elements
  [ ((Copilot..<<.), zipWith (\x y -> shiftL x (fromIntegral y)))
  , ((Copilot..>>.), zipWith (\x y -> shiftR x (fromIntegral y)))
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
