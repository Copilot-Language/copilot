{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
-- | Test copilot-core:Copilot.Core.Interpret.Eval.
--
-- The gist of this evaluation is in 'SemanticsP' and 'checkSemanticsP' which
-- evaluates an expression using Copilot's evaluator and compares it against
-- its expected meaning.
module Test.Copilot.Interpret.Eval where

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
                                             getPositive, oneof, suchThat,
                                             vectorOf)
import Text.PrettyPrint.HughesPJ            (render)

-- Internal imports: library modules being tested
import Copilot.Core.Expr        (Expr (Const, Drop, Op1, Op2, Op3),
                                 UExpr (UExpr))
import Copilot.Core.Operators   (Op1 (..), Op2 (..), Op3 (..))
import Copilot.Core.Spec        (Observer (..), Spec (..), Stream (Stream))
import Copilot.Core.Type        (Type (..), Typed (typeOf))
import Copilot.Interpret.Eval   (ExecTrace (interpObservers),
                                 ShowType (Haskell), eval)
import Copilot.PrettyPrint      (ppExpr)

-- Internal imports: auxiliary functions
import Test.Extra (apply1, apply2, apply3)
import Text.Read (readEither)

-- * Constants

-- | Max length of the traces being tested.
maxTraceLength :: Int
maxTraceLength = 200

-- | All unit tests for copilot-core:Copilot.Core.Interpret.Eval.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Core.Interpret.Eval"
    [ testProperty "eval Expr"           testEvalExpr
    , testProperty "eval Expr with Drop" testEvalExprWithDrop
    ]

-- * Individual tests

-- | Test for expression evaluation.
testEvalExpr :: Property
testEvalExpr =
  forAll (chooseInt (0, maxTraceLength)) $ \steps ->
  forAllShow arbitrarySemanticsP (semanticsShowK steps) $ \pair ->
  checkSemanticsP steps [] pair

-- | Test for expression evaluation with a drop.
testEvalExprWithDrop :: Property
testEvalExprWithDrop =
  forAll (chooseInt (0, maxTraceLength)) $ \steps ->
  forAllShow arbitrarySemanticsP (semanticsShowK steps) $ \pair ->
  forAllShow (arbitraryDrop pair) (semanticsShowK steps . snd) $ \(str, sem) ->
  checkSemanticsP steps [str] sem

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
  , SemanticsP <$> (arbitraryFloatingExpr     :: Gen (Semantics Float))
  , SemanticsP <$> (arbitraryFloatingExpr     :: Gen (Semantics Double))
  , SemanticsP <$> (arbitraryRealFracExpr     :: Gen (Semantics Float))
  , SemanticsP <$> (arbitraryRealFracExpr     :: Gen (Semantics Double))
  , SemanticsP <$> (arbitraryRealFloatExpr    :: Gen (Semantics Float))
  , SemanticsP <$> (arbitraryRealFloatExpr    :: Gen (Semantics Double))
  , SemanticsP <$> (arbitraryFractionalExpr   :: Gen (Semantics Float))
  , SemanticsP <$> (arbitraryFractionalExpr   :: Gen (Semantics Double))
  , SemanticsP <$> (arbitraryIntegralExpr     :: Gen (Semantics Int8))
  , SemanticsP <$> (arbitraryIntegralExpr     :: Gen (Semantics Int16))
  , SemanticsP <$> (arbitraryIntegralExpr     :: Gen (Semantics Int32))
  , SemanticsP <$> (arbitraryIntegralExpr     :: Gen (Semantics Int64))
  , SemanticsP <$> (arbitraryIntegralExpr     :: Gen (Semantics Word8))
  , SemanticsP <$> (arbitraryIntegralExpr     :: Gen (Semantics Word16))
  , SemanticsP <$> (arbitraryIntegralExpr     :: Gen (Semantics Word32))
  , SemanticsP <$> (arbitraryIntegralExpr     :: Gen (Semantics Word64))
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

-- | Generate an arbitrary drop by taking an expression, adding a number of
-- elements to it, and then dropping some.
arbitraryDrop :: SemanticsP -> Gen (Stream, SemanticsP)
arbitraryDrop (SemanticsP (expr, meaning)) = do
  -- Randomly generate a list of elements
  prependLength <- getPositive <$> arbitrary
  buffer        <- vectorOf prependLength arbitrary

  -- Build the stream with the buffer
  let streamId = 0
      stream   = Stream streamId buffer expr typeOf

  -- Select how many elements to drop from the stream (up to the length of the
  -- buffer)
  dropLength <- chooseInt (0, prependLength)

  -- Build a drop expression that drops those many elements, paired with its
  -- meaning.
  let expr'    = Drop typeOf (fromIntegral dropLength) streamId
      meaning' = drop dropLength buffer ++ meaning

  return (stream, SemanticsP (expr', meaning'))

-- ** Random Expr generators

-- | An arbitrary constant expression of any type, paired with its expected
-- meaning.
arbitraryConst :: (Arbitrary t, Typed t)
               => Gen (Expr t, [t])
arbitraryConst = (\v -> (Const typeOf v, repeat v)) <$> arbitrary

-- | An arbitrary boolean expression, paired with its expected meaning.
arbitraryBoolExpr :: Gen (Expr Bool, [Bool])
arbitraryBoolExpr =
  -- We use frequency instead of oneof because the random expression generator
  -- seems to generate expressions that are too large and the test fails due
  -- to running out of memory.
  frequency
    [ (10, arbitraryConst)

    , (5, apply1 <$> arbitraryBoolOp1 <*> arbitraryBoolExpr)

    , (1, apply2 <$> arbitraryBoolOp2
                 <*> arbitraryBoolExpr
                 <*> arbitraryBoolExpr)

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBoolExpr
                 <*> arbitraryBoolExpr)

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Expr Int8, [Int8])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Expr Int16, [Int16])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Expr Int32, [Int32])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Expr Int64, [Int64])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Expr Word8, [Word8])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Expr Word16, [Word16])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Expr Word32, [Word32])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryBitsExpr
                 <*> (arbitraryBitsExpr :: Gen (Expr Word64, [Word64])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Int8, [Int8])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Int16, [Int16])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Int32, [Int32])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Int64, [Int64])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Word8, [Word8])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Word16, [Word16])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Word32, [Word32])))

    , (1, apply2 <$> arbitraryEqOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Word64, [Word64])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Int8, [Int8])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Int16, [Int16])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Int32, [Int32])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Int64, [Int64])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Word8, [Word8])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Word16, [Word16])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Word32, [Word32])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryNumExpr
                 <*> (arbitraryNumExpr :: Gen (Expr Word64, [Word64])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryFloatingExpr
                 <*> (arbitraryFloatingExpr :: Gen (Expr Float, [Float])))

    , (1, apply2 <$> arbitraryOrdOp2
                 <*> arbitraryFloatingExpr
                 <*> (arbitraryFloatingExpr :: Gen (Expr Double, [Double])))

    , (1, apply3 <$> arbitraryITEOp3
                 <*> arbitraryBoolExpr
                 <*> arbitraryBoolExpr
                 <*> arbitraryBoolExpr)
    ]

-- | An arbitrary numeric expression, paired with its expected meaning.
arbitraryNumExpr :: (Arbitrary t, Typed t, Num t)
                 => Gen (Expr t, [t])
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
arbitraryFloatingExpr :: (Arbitrary t, Typed t, Floating t)
                      => Gen (Expr t, [t])
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

-- | An arbitrary realfrac expression, paired with its expected meaning.
arbitraryRealFracExpr :: (Arbitrary t, Typed t, RealFrac t)
                      => Gen (Expr t, [t])
arbitraryRealFracExpr =
  -- We use frequency instead of oneof because the random expression generator
  -- seems to generate expressions that are too large and the test fails due
  -- to running out of memory.
  frequency
    [ (10, arbitraryConst)

    , (2, apply1 <$> arbitraryRealFracOp1 <*> arbitraryRealFracExpr)

    , (5, apply1 <$> arbitraryNumOp1      <*> arbitraryRealFracExpr)

    , (1, apply2 <$> arbitraryNumOp2
                 <*> arbitraryRealFracExpr
                 <*> arbitraryRealFracExpr)

    , (1, apply3 <$> arbitraryITEOp3
                 <*> arbitraryBoolExpr
                 <*> arbitraryRealFracExpr
                 <*> arbitraryRealFracExpr)
    ]

-- | An arbitrary realfloat expression, paired with its expected meaning.
arbitraryRealFloatExpr :: (Arbitrary t, Typed t, RealFloat t)
                       => Gen (Expr t, [t])
arbitraryRealFloatExpr =
  -- We use frequency instead of oneof because the random expression generator
  -- seems to generate expressions that are too large and the test fails due
  -- to running out of memory.
  frequency
    [ (10, arbitraryConst)

    , (2, apply1 <$> arbitraryNumOp1 <*> arbitraryRealFloatExpr)

    , (5, apply2 <$> arbitraryRealFloatOp2
                 <*> arbitraryRealFloatExpr
                 <*> arbitraryRealFloatExpr)

    , (1, apply2 <$> arbitraryNumOp2
                 <*> arbitraryRealFloatExpr
                 <*> arbitraryRealFloatExpr)

    , (1, apply3 <$> arbitraryITEOp3
                 <*> arbitraryBoolExpr
                 <*> arbitraryRealFloatExpr
                 <*> arbitraryRealFloatExpr)
    ]

-- | An arbitrary fractional expression, paired with its expected meaning.
--
-- We add the constraint Eq because we sometimes need to make sure numbers are
-- not zero.
arbitraryFractionalExpr :: (Arbitrary t, Typed t, Fractional t, Eq t)
                        => Gen (Expr t, [t])
arbitraryFractionalExpr =
  -- We use frequency instead of oneof because the random expression generator
  -- seems to generate expressions that are too large and the test fails due
  -- to running out of memory.
  frequency
    [ (10, arbitraryConst)

    , (5, apply1 <$> arbitraryFractionalOp1 <*> arbitraryFractionalExpr)

    , (5, apply1 <$> arbitraryNumOp1 <*> arbitraryFractionalExpr)

    , (2, apply2 <$> arbitraryFractionalOp2
                 <*> arbitraryFractionalExpr
                 <*> arbitraryFractionalExprNonZero)

    , (1, apply3 <$> arbitraryITEOp3
                 <*> arbitraryBoolExpr
                 <*> arbitraryFractionalExpr
                 <*> arbitraryFractionalExpr)
    ]
  where

    -- Generator for fractional expressions that are never zero.
    --
    -- The list is infinite, so this generator checks up to maxTraceLength
    -- elements.
    arbitraryFractionalExprNonZero = arbitraryFractionalExpr
      `suchThat` (notElem 0 . take maxTraceLength . snd)

-- | An arbitrary integral expression, paired with its expected meaning.
--
-- We add the constraint Eq because we sometimes need to make sure numbers are
-- not zero.
arbitraryIntegralExpr :: (Arbitrary t, Typed t, Integral t, Eq t)
                      => Gen (Expr t, [t])
arbitraryIntegralExpr =
  -- We use frequency instead of oneof because the random expression generator
  -- seems to generate expressions that are too large and the test fails due
  -- to running out of memory.
  frequency
    [ (10, arbitraryConst)

    , (5, apply1 <$> arbitraryNumOp1 <*> arbitraryIntegralExpr)

    , (2, apply2 <$> arbitraryNumOp2
                 <*> arbitraryIntegralExpr
                 <*> arbitraryIntegralExpr)

    , (2, apply2 <$> arbitraryIntegralOp2
                 <*> arbitraryIntegralExpr
                 <*> arbitraryIntegralExprNonZero)

    , (1, apply3 <$> arbitraryITEOp3
                 <*> arbitraryBoolExpr
                 <*> arbitraryIntegralExpr
                 <*> arbitraryIntegralExpr)
    ]
  where

    -- Generator for integral expressions that are never zero.
    --
    -- The list is infinite, so this generator checks up to maxTraceLength
    -- elements.
    arbitraryIntegralExprNonZero = arbitraryIntegralExpr
      `suchThat` (notElem 0 . take maxTraceLength . snd)

-- | An arbitrary Bits expression, paired with its expected meaning.
arbitraryBitsExpr :: (Arbitrary t, Typed t, Bits t)
                  => Gen (Expr t, [t])
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
                          => Gen (Expr t, [t])
arbitraryBitsIntegralExpr =
      -- We use frequency instead of oneof because the random expression
      -- generator seems to generate expressions that are too large and the
      -- test fails due to running out of memory.
      frequency
        [ (10, arbitraryConst)

        , (2, apply1 <$> arbitraryNumOp1 <*> arbitraryBitsIntegralExpr)

        , (1, apply2 <$> arbitraryNumOp2
                     <*> arbitraryBitsIntegralExpr
                     <*> arbitraryBitsIntegralExpr)

        , (5, apply2 <$> arbitraryBitsIntegralOp2
                     <*> arbitraryBitsIntegralExpr
                     <*> arbitraryBitsIntegralExprConstPos)

        , (1, apply3 <$> arbitraryITEOp3
                     <*> arbitraryBoolExpr
                     <*> arbitraryBitsIntegralExpr
                     <*> arbitraryBitsIntegralExpr)
        ]
  where

    -- Generator for constant bit integral expressions that, when converted to
    -- type 't', result in a positive number. We use a constant generator, as
    -- opposed to a generator based on the more comprehensive
    -- arbitraryBitsIntegralExpr, because the latter runs out of memory easily
    -- when nested and filtered with suchThat.
    arbitraryBitsIntegralExprConstPos =
        (\v -> (Const typeOf v, repeat v)) <$> intThatFits
      where
        -- In this context:
        --
        -- intThatFits :: Gen t
        intThatFits =
          suchThat arbitrary ((> 0) . (\x -> (fromIntegral x) :: Int))

-- ** Operators

-- *** Op 1

-- | Generator for arbitrary boolean operators with arity 1, paired with their
-- expected meaning.
arbitraryBoolOp1 :: Gen (Expr Bool -> Expr Bool, [Bool] -> [Bool])
arbitraryBoolOp1 = elements
  [ (Op1 Not, fmap not)
  ]

-- | Generator for arbitrary numeric operators with arity 1, paired with their
-- expected meaning.
arbitraryNumOp1 :: (Typed t, Num t)
                => Gen (Expr t -> Expr t, [t] -> [t])
arbitraryNumOp1 = elements
  [ (Op1 (Abs typeOf),  fmap abs)
  , (Op1 (Sign typeOf), fmap signum)
  ]

-- | Generator for arbitrary floating point operators with arity 1, paired with
-- their expected meaning.
arbitraryFloatingOp1 :: (Typed t, Floating t)
                     => Gen (Expr t -> Expr t, [t] -> [t])
arbitraryFloatingOp1 = elements
  [ (Op1 (Exp typeOf),   fmap exp)
  , (Op1 (Sqrt typeOf),  fmap sqrt)
  , (Op1 (Log typeOf),   fmap log)
  , (Op1 (Sin typeOf),   fmap sin)
  , (Op1 (Tan typeOf),   fmap tan)
  , (Op1 (Cos typeOf),   fmap cos)
  , (Op1 (Asin typeOf),  fmap asin)
  , (Op1 (Atan typeOf),  fmap atan)
  , (Op1 (Acos typeOf),  fmap acos)
  , (Op1 (Sinh typeOf),  fmap sinh)
  , (Op1 (Tanh typeOf),  fmap tanh)
  , (Op1 (Cosh typeOf),  fmap cosh)
  , (Op1 (Asinh typeOf), fmap asinh)
  , (Op1 (Atanh typeOf), fmap atanh)
  , (Op1 (Acosh typeOf), fmap acosh)
  ]

-- | Generator for arbitrary realfrac operators with arity 1, paired with their
-- expected meaning.
arbitraryRealFracOp1 :: (Typed t, RealFrac t)
                     => Gen (Expr t -> Expr t, [t] -> [t])
arbitraryRealFracOp1 = elements
    [ (Op1 (Ceiling typeOf), fmap (fromIntegral . idI . ceiling))
    , (Op1 (Floor typeOf), fmap (fromIntegral . idI . floor))
    ]
  where
    -- Auxiliary function to help the compiler determine which integral type
    -- the result of ceiling must be converted to. An Integer ensures that the
    -- result fits and there is no loss of precision due to the intermediate
    -- casting.
    idI :: Integer -> Integer
    idI = id

-- | Generator for arbitrary fractional operators with arity 1, paired with
-- their expected meaning.
arbitraryFractionalOp1 :: (Typed t, Fractional t)
                       => Gen (Expr t -> Expr t, [t] -> [t])
arbitraryFractionalOp1 = elements
  [ (Op1 (Recip typeOf), fmap recip)
  ]

-- | Generator for arbitrary bitwise operators with arity 1, paired with their
-- expected meaning.
arbitraryBitsOp1 :: (Typed t, Bits t)
                 => Gen (Expr t -> Expr t, [t] -> [t])
arbitraryBitsOp1 = elements
  [ (Op1 (BwNot typeOf), fmap complement)
  ]

-- *** Op 2

-- | Generator for arbitrary boolean operators with arity 2, paired with their
-- expected meaning.
arbitraryBoolOp2 :: Gen ( Expr Bool -> Expr Bool -> Expr Bool
                        , [Bool] -> [Bool] -> [Bool]
                        )
arbitraryBoolOp2 = elements
  [ (Op2 And, zipWith (&&))
  , (Op2 Or,  zipWith (||))
  ]

-- | Generator for arbitrary numeric operators with arity 2, paired with their
-- expected meaning.
arbitraryNumOp2 :: (Typed t, Num t)
                => Gen (Expr t -> Expr t -> Expr t, [t] -> [t] -> [t])
arbitraryNumOp2 = elements
  [ (Op2 (Add typeOf), zipWith (+))
  , (Op2 (Sub typeOf), zipWith (-))
  , (Op2 (Mul typeOf), zipWith (*))
  ]

-- | Generator for arbitrary integral operators with arity 2, paired with their
-- expected meaning.
arbitraryIntegralOp2 :: (Typed t, Integral t)
                     => Gen (Expr t -> Expr t -> Expr t, [t] -> [t] -> [t])
arbitraryIntegralOp2 = elements
  [ (Op2 (Mod typeOf), zipWith mod)
  , (Op2 (Div typeOf), zipWith quot)
  ]

-- | Generator for arbitrary fractional operators with arity 2, paired with
-- their expected meaning.
arbitraryFractionalOp2 :: (Typed t, Fractional t)
                       => Gen (Expr t -> Expr t -> Expr t, [t] -> [t] -> [t])
arbitraryFractionalOp2 = elements
  [ (Op2 (Fdiv typeOf), zipWith (/))
  ]

-- | Generator for arbitrary floating point operators with arity 2, paired with
-- their expected meaning.
arbitraryFloatingOp2 :: (Typed t, Floating t)
                     => Gen (Expr t -> Expr t -> Expr t, [t] -> [t] -> [t])
arbitraryFloatingOp2 = elements
  [ (Op2 (Pow typeOf),  zipWith (**))
  , (Op2 (Logb typeOf), zipWith logBase)
  ]

-- | Generator for arbitrary floating point operators with arity 2, paired with
-- their expected meaning.
arbitraryRealFloatOp2 :: (Typed t, RealFloat t)
                      => Gen (Expr t -> Expr t -> Expr t, [t] -> [t] -> [t])
arbitraryRealFloatOp2 = elements
  [ (Op2 (Atan2 typeOf), zipWith atan2)
  ]

-- | Generator for arbitrary equality operators with arity 2, paired with their
-- expected meaning.
arbitraryEqOp2 :: (Typed t, Eq t)
               => Gen (Expr t -> Expr t -> Expr Bool, [t] -> [t] -> [Bool])
arbitraryEqOp2 = elements
  [ (Op2 (Eq typeOf), zipWith (==))
  , (Op2 (Ne typeOf), zipWith (/=))
  ]

-- | Generator for arbitrary ordering operators with arity 2, paired with their
-- expected meaning.
arbitraryOrdOp2 :: (Typed t, Ord t)
                => Gen (Expr t -> Expr t -> Expr Bool, [t] -> [t] -> [Bool])
arbitraryOrdOp2 = elements
  [ (Op2 (Le typeOf), zipWith (<=))
  , (Op2 (Lt typeOf), zipWith (<))
  , (Op2 (Ge typeOf), zipWith (>=))
  , (Op2 (Gt typeOf), zipWith (>))
  ]

-- | Generator for arbitrary bitwise operators with arity 2, paired with their
-- expected meaning.
arbitraryBitsOp2 :: (Typed t, Bits t)
                 => Gen (Expr t -> Expr t -> Expr t, [t] -> [t] -> [t])
arbitraryBitsOp2 = elements
  [ (Op2 (BwAnd typeOf), zipWith (.&.))
  , (Op2 (BwOr typeOf),  zipWith (.|.))
  , (Op2 (BwXor typeOf), zipWith xor)
  ]

-- | Generator for arbitrary bit shifting operators with arity 2, paired with
-- their expected meaning.
--
-- This generator is a bit more strict in its type signature than the
-- underlying bit-shifting operators being tested, since it enforces both the
-- value being manipulated and the value that indicates how much to shift by to
-- have the same type.
arbitraryBitsIntegralOp2 :: (Typed t, Bits t, Integral t)
                         => Gen (Expr t -> Expr t -> Expr t, [t] -> [t] -> [t])
arbitraryBitsIntegralOp2 = elements
  [ (Op2 (BwShiftL typeOf typeOf), zipWith (\x y -> shiftL x (fromIntegral y)))
  , (Op2 (BwShiftR typeOf typeOf), zipWith (\x y -> shiftR x (fromIntegral y)))
  ]

-- *** Op 3

-- | Generator for if-then-else operator (with arity 3), paired with its
-- expected meaning.
--
-- Although this is constant and there is nothing arbitrary, we use the same
-- structure and naming convention as with others for simplicity.
arbitraryITEOp3 :: (Arbitrary t, Typed t)
                => Gen ( Expr Bool -> Expr t -> Expr t -> Expr t
                       , [Bool] -> [t] -> [t] -> [t]
                       )
arbitraryITEOp3 = return
  (Op3 (Mux typeOf), zipWith3 (\x y z -> if x then y else z))

-- * Semantics

-- | Type that pairs an expression with its meaning as an infinite stream.
type Semantics t = (Expr t, [t])

-- | A phantom semantics pair is an existential type that encloses an
-- expression and its expected meaning as an infinite list of values.
--
-- It is needed by the arbitrary expression generator, to create a
-- heterogeneous list.
data SemanticsP = forall t
                . (Typeable t, Read t, Eq t, Show t, Typed t, Arbitrary t)
                => SemanticsP
  { semanticsPair :: (Expr t, [t])
  }

-- | Show function for test triplets that limits the accompanying list
-- to a certain length.
semanticsShowK :: Int -> SemanticsP -> String
semanticsShowK steps (SemanticsP (expr, exprList)) =
    show (showType ty, render $ ppExpr expr, take steps exprList)

  where

    -- Type of the expression. The type is enforced by _u below.
    ty = typeOf

    -- We want to show the type. To help GHC determine that the type t is the
    -- same as the expression's (expr), we use an UExpr, which has an
    -- additional constraint. This definition serves no other purpose than to
    -- help enforce that constraint.
    _u = UExpr ty expr

-- | Check that the expression in the semantics pair is evaluated to the given
-- list, up to a number of steps.
--
-- Some operations will overflow and return NaN. Because comparing any NaN
-- will, as per IEEE 754, always fail (i.e., return False), we handle that
-- specific case by stating that the test succeeds if any expected values
-- is NaN.
checkSemanticsP :: Int -> [Stream] -> SemanticsP -> Bool
checkSemanticsP steps streams (SemanticsP (expr, exprList)) =
    any isNaN' expectation || resultValues == expectation
  where
    -- Limit expectation to the number of evaluation steps.
    expectation = pure <$> take steps exprList

    -- Obtain the results by looking up the observer in the spec
    -- and parsing the results into Haskell values.
    resultValues = fmap readResult results
    results      = lookupWithDefault testObserverName []
                 $ interpObservers trace

    -- Spec with just one observer of one expression.
    trace     = eval Haskell steps spec
    spec      = Spec streams observers [] []
    observers = [Observer testObserverName expr typeOf]

    -- Fixed name for the observer. Used to obtain the result from the
    -- trace. It should be the only observer in the trace.
    testObserverName :: String
    testObserverName = "res"

    -- | Is NaN with Eq requirement only.
    isNaN' :: Eq a => a -> Bool
    isNaN' x = x /= x

-- * Auxiliary

-- | Read a Haskell value from the output of the evaluator.
readResult :: Read a => String -> Either String a
readResult = readEither . readResult'
  where
    readResult' :: String -> String
    readResult' "false" = "False"
    readResult' "true"  = "True"
    readResult' s       = s

-- | Variant of 'lookup' with an additional default value returned when the key
-- provided is not found in the map.
lookupWithDefault :: Ord k => k -> v -> [(k, v)] -> v
lookupWithDefault k def = fromMaybe def . lookup k

-- | Show Copilot Core type.
showType :: Type a -> String
showType t' = case t' of
    Bool   -> "Bool"
    Int8   -> "Int8"
    Int16  -> "Int16"
    Int32  -> "Int32"
    Int64  -> "Int64"
    Word8  -> "Word8"
    Word16 -> "Word16"
    Word32 -> "Word32"
    Word64 -> "Word64"
    Float  -> "Float"
    Double -> "Double"
    Array t -> "Array " ++ showType t
    Struct _ -> "Struct"
