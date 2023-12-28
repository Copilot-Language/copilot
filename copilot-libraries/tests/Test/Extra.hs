-- The following warning is disabled due to a necessary instance of SatResult
-- defined in this module.
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Testing facilities based on the connection to SMT solvers and the
-- interpreter.
--
-- This module provides functions to check if a stream meets some expectation,
-- and to generate random streams.
--
-- The test against an expectation can be done by checking a boolean stream
-- with copilot-theorem and comparing it with the expected validity, or by
-- evaluating a stream with the interpreter and comparing it with the expected
-- list of values it should contain.
module Test.Extra
    (
      -- * Test using copilot-theorem
      testWithTheorem

      -- * Test using copilot-interpreter
    , testWithInterpreter

      -- * Random Stream generators
    , arbitraryConst
    , arbitraryBoolExpr
    , arbitraryNumExpr
    , arbitraryFloatingExpr
    , arbitraryBitsExpr
    )
  where

-- External imports
import Control.Arrow           ((***))
import Control.Monad           (void)
import Data.Bits               (Bits, complement, xor, (.&.), (.|.))
import Data.Int                (Int16, Int32, Int64, Int8)
import Data.Maybe              (fromMaybe)
import Data.Word               (Word16, Word32, Word64, Word8)
import Test.QuickCheck         (Arbitrary, Gen, Property, arbitrary, chooseInt,
                                elements, forAll, forAllShow, frequency)
import Test.QuickCheck.Monadic (monadicIO, run)

-- External imports: Copilot
import           Copilot.Interpret.Eval              (ExecTrace (interpObservers),
                                                      ShowType (Haskell), eval)
import           Copilot.Language                    (Spec, Stream, Typed,
                                                      observer, prop)
import qualified Copilot.Language                    as Copilot
import qualified Copilot.Language.Operators.Boolean  as Copilot
import qualified Copilot.Language.Operators.Constant as Copilot
import qualified Copilot.Language.Operators.Eq       as Copilot
import qualified Copilot.Language.Operators.Mux      as Copilot
import qualified Copilot.Language.Operators.Ord      as Copilot
import           Copilot.Language.Reify              (reify)
import           Copilot.Theorem.What4               (SatResult (..),
                                                      Solver (..), prove)

-- * Test using copilot-theorem

-- | Define a QuickCheck property based on a generator of boolean streams and
-- their validity as Copilot properties.
--
-- Uses the connection to copilot-theorem to determine whether the expectation
-- is the same as what copilot-theorem determines.
testWithTheorem :: Gen (Stream Bool, SatResult) -> Property
testWithTheorem gen =
  forAll gen $ \(stream, expectation) -> do
    let propName :: String
        propName = "prop"

        spec :: Spec
        spec = void $ prop propName $ Copilot.forAll stream

    monadicIO $ run $ checkResult Z3 propName spec expectation

-- | Check that the solver's satisfiability result for the given Copilot
-- property in the given spec matches the expectation.
checkResult :: Solver -> String -> Spec -> SatResult -> IO Bool
checkResult solver propName spec expectation = do
  spec' <- reify spec
  results <- prove solver spec'

  -- Find the satisfiability result for propName.
  let propResult = lookup propName results

  -- The following check also works for the case in which the property name
  -- does not exist in the results, in which case the lookup returns 'Nothing'.
  return $ propResult == Just expectation

-- | Equality for 'SatResult'.
--
-- This is an orphan instance. We suppress the warning that GHC would normally
-- produce with a GHC option at the top.
instance Eq SatResult where
  Valid   == Valid   = True
  Invalid == Invalid = True
  Unknown == Unknown = True
  _       == _       = False

-- * Testing facilities based on copilot-interpreter

-- | Max length of the traces being tested.
maxTraceLength :: Int
maxTraceLength = 200

-- | Define a QuickCheck property based on an interpretation of the stream and
-- a comparison with a list, up to a given 'maxTraceLength'.
--
-- Uses the connection to copilot-interpreter to determine whether the
-- expectation is the same as what the interpreter determines.
testWithInterpreter :: (Eq t, Read t, Typed t)
                    => Gen (Stream t, [t])
                    -> Property
testWithInterpreter stream =
  forAll (chooseInt (0, maxTraceLength)) $ \steps ->
  forAllShow stream (testPairShowK steps) $ \pair ->
  monadicIO $ run (checkTestPairP steps pair)

-- | Show function for test pairs that limits the accompanying list to a
-- certain length.
testPairShowK :: Show t => Int -> (Stream t, [t]) -> String
testPairShowK steps (_expr, exprList) =
  show ("Cannot show stream", take steps exprList)

-- | Check that the expression in the test pair is evaluated to the given list,
-- up to a number of steps.
--
-- Some operations will overflow and return NaN. Because comparing any NaN
-- will, as per IEEE 754, always fail (i.e., return False), we handle that
-- specific case by stating that the test succeeds if any expected values is
-- NaN.
checkTestPairP :: (Eq t, Read t, Typed t, Show t)
               => Int
               -> (Stream t, [t])
               -> IO Bool
checkTestPairP steps (expr, exprList) = do
    -- Spec with just one observer of one expression.
    --
    -- We need to help GHC figure out the type of spec.
    let spec :: Spec
        spec = observer testObserverName expr

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

    -- | Read a Haskell value from the output of the evaluator.
    readResult :: Read a => String -> a
    readResult = read . readResult'
      where
        readResult' :: String -> String
        readResult' "false" = "False"
        readResult' "true"  = "True"
        readResult' s       = s

-- * Random Stream generators

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

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Int8, [Int8])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Int16, [Int16])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Int32, [Int32])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Int64, [Int64])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Word8, [Word8])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Word16, [Word16])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Word32, [Word32])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Stream Word64, [Word64])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryFloatingExpr
                 <*> (arbitraryFloatingExpr :: Gen (Stream Float, [Float])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryFloatingExpr
                 <*> (arbitraryFloatingExpr :: Gen (Stream Double, [Double])))

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

-- | An arbitrary floating point expression, paired with its expected meaning.
arbitraryFloatingExpr :: (Arbitrary t, Typed t, Floating t, Eq t)
                      => Gen (Stream t, [t])
arbitraryFloatingExpr =
  -- We use frequency instead of oneof because the random expression generator
  -- seems to generate expressions that are too large and the test fails due
  -- to running out of memory.
  frequency
    [ (10, arbitraryConst)

    , (5, apply1 <$> arbitraryFloatingOp1 <*> arbitraryFloatingExpr)

    , (5, apply1 <$> arbitraryNumOp1 <*> arbitraryFloatingExpr)

    , (2, apply2 <$> arbitraryFloatingOp2
                 <*> arbitraryFloatingExpr
                 <*> arbitraryFloatingExpr)

    , (2, apply2 <$> arbitraryNumOp2
                 <*> arbitraryFloatingExpr
                 <*> arbitraryFloatingExpr)

    , (1, apply3 <$> arbitraryITEOp3
                 <*> arbitraryBoolExpr
                 <*> arbitraryFloatingExpr
                 <*> arbitraryFloatingExpr)
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

-- ** Operators

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

-- | Generator for arbitrary floating point operators with arity 1, paired with
-- their expected meaning.
arbitraryFloatingOp1 :: (Typed t, Floating t, Eq t)
                     => Gen (Stream t -> Stream t, [t] -> [t])
arbitraryFloatingOp1 = elements
  [ (exp,   fmap exp)
  , (sqrt,  fmap sqrt)
  , (log,   fmap log)
  , (sin,   fmap sin)
  , (tan,   fmap tan)
  , (cos,   fmap cos)
  , (asin,  fmap asin)
  , (atan,  fmap atan)
  , (acos,  fmap acos)
  , (sinh,  fmap sinh)
  , (tanh,  fmap tanh)
  , (cosh,  fmap cosh)
  , (asinh, fmap asinh)
  , (atanh, fmap atanh)
  , (acosh, fmap acosh)
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

-- | Generator for arbitrary floating point operators with arity 2, paired with
-- their expected meaning.
arbitraryFloatingOp2 :: (Typed t, Floating t, Eq t)
                     => Gen ( Stream t -> Stream t -> Stream t
                            , [t] -> [t] -> [t]
                            )
arbitraryFloatingOp2 = elements
  [ ((**),    zipWith (**))
  , (logBase, zipWith logBase)
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

-- | Generator for arbitrary ordering operators with arity 2, paired with their
-- expected meaning.
arbitraryOrdOp2 :: (Typed t, Ord t)
                => Gen ( Stream t -> Stream t -> Stream Bool
                       , [t] -> [t] -> [Bool]
                       )
arbitraryOrdOp2 = elements
  [ ((Copilot.<=), zipWith (<=))
  , ((Copilot.<),  zipWith (<))
  , ((Copilot.>=), zipWith (>=))
  , ((Copilot.>),  zipWith (>))
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

-- * Auxiliary

-- | Apply a tuple with two functions to a tuple of arguments.
apply1 :: (a1 -> b1, a2 -> b2) -- ^ Pair with functions
       -> (a1, a2)             -- ^ Pair with arguments
       -> (b1, b2)             -- ^ Pair with results
apply1 = uncurry (***)

-- | Apply a tuple with two functions on two arguments to their tupled
-- arguments.
apply2 :: (a1 -> b1 -> c1, a2 -> b2 -> c2) -- ^ Pair with functions
       -> (a1, a2)                         -- ^ Pair with first arguments
       -> (b1, b2)                         -- ^ Pair with second arguments
       -> (c1, c2)                         -- ^ Pair with results
apply2 fs = apply1 . apply1 fs

-- | Apply a tuple with two functions on three arguments to their tupled
-- arguments.
apply3 :: (a1 -> b1 -> c1 -> d1, a2 -> b2 -> c2 -> d2)
                    -- ^ Pair with functions
       -> (a1, a2)  -- ^ Pair with first arguments
       -> (b1, b2)  -- ^ Pair with second arguments
       -> (c1, c2)  -- ^ Pair with third arguments
       -> (d1, d2)  -- ^ Pair with results
apply3 fs = apply2 . apply1 fs

-- | Variant of 'lookup' with an additional default value returned when the key
-- provided is not found in the map.
lookupWithDefault :: Ord k => k -> v -> [(k, v)] -> v
lookupWithDefault k def = fromMaybe def . lookup k
