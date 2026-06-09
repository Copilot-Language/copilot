-- | Test copilot-theorem:Copilot.Theorem.Kind2.
--
-- These tests require the @kind2@ executable (version 1.0 or newer) to be in
-- the @PATH@.
module Test.Copilot.Theorem.Kind2 where

-- External imports
import Control.Monad.Writer               (tell)
import Data.Int                           (Int32)
import Test.Framework                     (Test, testGroup)
import Test.Framework.Providers.HUnit     (testCase)
import Test.HUnit                         (Assertion, assertEqual)

-- External imports: Copilot
import           Copilot.Core.Expr      (Expr (Const, Drop, ExternVar, Op1,
                                               Op2), Id)
import           Copilot.Core.Operators (Op1 (..), Op2 (..))
import           Copilot.Core.Spec      (Spec (..), Stream (..))
import qualified Copilot.Core.Spec      as Copilot
import           Copilot.Core.Type      (Typed (typeOf))

-- Internal imports: Modules being tested
import Copilot.Theorem.Kind2.Prover (def, kind2Prover)
import Copilot.Theorem.Prove        (Action (..), PropId, prove)

-- * Constants

-- | Unit tests for copilot-theorem:Copilot.Theorem.Kind2.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Theorem.Kind2"
    [ testCase "Prove that true is valid"
        testTrueValid
    , testCase "Prove that false is invalid"
        testFalseInvalid
    , testCase "Prove that an existentially quantified true is valid"
        testExistsTrueValid
    , testCase "Prove that an existentially quantified false is invalid"
        testExistsFalseInvalid
    , testCase "Prove a valid property of a recursive stream"
        testCounterGt1
    , testCase "Disprove an invalid property of a recursive stream"
        testCounterLt255
    , testCase "Prove a valid existential property of a recursive stream"
        testCounterExistsGt3
    , testCase "Prove a valid property of an external stream"
        testExternEq
    , testCase "Prove a property using an assumption"
        testAssumption
    , testCase "Disprove a property when its assumption is not used"
        testNoAssumption
    , testCase "Prove a valid property of a real-valued stream"
        testRealValued
    , testCase "Prove a valid property of a boolean stream"
        testBoolStream
    ]

-- * Individual tests

-- | Test that Kind2 is able to prove the following property valid:
-- @
--   constant True
-- @
testTrueValid :: Assertion
testTrueValid =
    checkProve True propName spec
  where
    propName = "prop"
    spec     = forallPropSpec propName [] $ Const typeOf True

-- | Test that Kind2 is able to prove the following property invalid:
-- @
--   constant False
-- @
testFalseInvalid :: Assertion
testFalseInvalid =
    checkProve False propName spec
  where
    propName = "prop"
    spec     = forallPropSpec propName [] $ Const typeOf False

-- | Test that Kind2 is able to prove the following property valid:
-- @
--   exists: constant True
-- @
--
-- Existentially quantified properties are encoded by asking Kind2 to check
-- the negation of the property, so this test exercises the case in which
-- Kind2 reports that a property is falsifiable.
testExistsTrueValid :: Assertion
testExistsTrueValid =
    checkProve True propName spec
  where
    propName = "prop"
    spec     = existsPropSpec propName [] $ Const typeOf True

-- | Test that Kind2 is able to prove the following property invalid:
-- @
--   exists: constant False
-- @
testExistsFalseInvalid :: Assertion
testExistsFalseInvalid =
    checkProve False propName spec
  where
    propName = "prop"
    spec     = existsPropSpec propName [] $ Const typeOf False

-- | Test that Kind2 is able to prove the following property valid:
-- @
--   let x = [2] ++ (x + 1)
--   in forAll (x > 1)
-- @
testCounterGt1 :: Assertion
testCounterGt1 =
    checkProve True propName spec
  where
    propName = "gt1"
    spec     = forallPropSpec propName [counterStream] $
      Op2 (Gt typeOf) counterExpr (constI32 1)

-- | Test that Kind2 is able to prove the following property invalid:
-- @
--   let x = [2] ++ (x + 1)
--   in forAll (x < 255)
-- @
--
-- This test exercises the case in which Kind2 reports that a property is
-- falsifiable, which, for systems in Kind2's native input format, also makes
-- Kind2 produce output that is not well-formed XML.
testCounterLt255 :: Assertion
testCounterLt255 =
    checkProve False propName spec
  where
    propName = "lt255"
    spec     = forallPropSpec propName [counterStream] $
      Op2 (Lt typeOf) counterExpr (constI32 255)

-- | Test that Kind2 is able to prove the following property valid:
-- @
--   let x = [2] ++ (x + 1)
--   in exists (x > 3)
-- @
testCounterExistsGt3 :: Assertion
testCounterExistsGt3 =
    checkProve True propName spec
  where
    propName = "gt3"
    spec     = existsPropSpec propName [counterStream] $
      Op2 (Gt typeOf) counterExpr (constI32 3)

-- | Test that Kind2 is able to prove the following property valid:
-- @
--   let e = extern "ext" Nothing
--   in forAll (e == e)
-- @
testExternEq :: Assertion
testExternEq =
    checkProve True propName spec
  where
    propName = "eq"
    extECopy = ExternVar typeOf "ext" Nothing :: Expr Int32
    spec     = forallPropSpec propName [] $ Op2 (Eq typeOf) extECopy extECopy

-- | Test that Kind2 is able to prove the property @lt20@ valid when assuming
-- the property @lt10@, in the following specification:
-- @
--   let x = [2] ++ (x + 1)
--   in forAll (x < 10)  -- lt10
--      forAll (x < 20)  -- lt20
-- @
testAssumption :: Assertion
testAssumption = do
    result <- prove spec "lt20" $
      tell [Assume "lt10", Check (kind2Prover def)]
    assertEqual "proof result for lt20 (assuming lt10)" True result
  where
    spec = assumptionSpec

-- | Test that Kind2 is able to prove the property @lt20@ invalid when the
-- assumption @lt10@ is not used, in the same specification as
-- 'testAssumption'.
testNoAssumption :: Assertion
testNoAssumption = do
    result <- prove spec "lt20" $
      tell [Check (kind2Prover def)]
    assertEqual "proof result for lt20 (without assumptions)" False result
  where
    spec = assumptionSpec

-- | Test that Kind2 is able to prove the following property valid:
-- @
--   let y = [0.5] ++ y
--   in forAll (y > 0.0)
-- @
testRealValued :: Assertion
testRealValued =
    checkProve True propName spec
  where
    propName = "pos"

    streamId' :: Id
    streamId' = 0

    stream :: Stream
    stream = Stream
      { streamId       = streamId'
      , streamBuffer   = [0.5 :: Double]
      , streamExpr     = Drop typeOf 0 streamId'
      , streamExprType = typeOf
      }

    spec = forallPropSpec propName [stream] $
      Op2 (Gt typeOf) (Drop typeOf 0 streamId' :: Expr Double)
                      (Const typeOf (0.0 :: Double))

-- | Test that Kind2 is able to prove the following property valid:
-- @
--   let b = [True] ++ (b && constant True)
--   in forAll (not (not b))
-- @
testBoolStream :: Assertion
testBoolStream =
    checkProve True propName spec
  where
    propName = "tt"

    streamId' :: Id
    streamId' = 0

    stream :: Stream
    stream = Stream
      { streamId       = streamId'
      , streamBuffer   = [True]
      , streamExpr     = Op2 And (Drop typeOf 0 streamId') (Const typeOf True)
      , streamExprType = typeOf
      }

    spec = forallPropSpec propName [stream] $
      Op1 Not (Op1 Not (Drop typeOf 0 streamId'))

-- * Auxiliary

-- | Check that proving the given property of the given spec with the Kind2
-- prover produces the expected result.
checkProve :: Bool -> PropId -> Spec -> Assertion
checkProve expectation propName spec = do
  result <- prove spec propName $ tell [Check (kind2Prover def)]
  assertEqual ("proof result for property " ++ propName) expectation result

-- | A stream of 'Int32' defined by:
-- @
--   x = [2] ++ (x + 1)
-- @
counterStream :: Stream
counterStream = Stream
  { streamId       = counterStreamId
  , streamBuffer   = [2 :: Int32]
  , streamExpr     = Op2 (Add typeOf) counterExpr (constI32 1)
  , streamExprType = typeOf
  }

-- | Id of 'counterStream'.
counterStreamId :: Id
counterStreamId = 0

-- | The current value of 'counterStream'.
counterExpr :: Expr Int32
counterExpr = Drop typeOf 0 counterStreamId

-- | A constant 'Int32' expression.
constI32 :: Int32 -> Expr Int32
constI32 = Const typeOf

-- | A specification with the 'counterStream' and two properties, @lt10@ and
-- @lt20@, stating that the values of the stream are smaller than 10 and 20,
-- respectively. Both properties are invalid on their own, but @lt20@ holds
-- if @lt10@ is assumed.
assumptionSpec :: Spec
assumptionSpec = Spec
  [counterStream]
  []
  []
  [ Copilot.Property "lt10" $ Copilot.Forall $
      Op2 (Lt typeOf) counterExpr (constI32 10)
  , Copilot.Property "lt20" $ Copilot.Forall $
      Op2 (Lt typeOf) counterExpr (constI32 20)
  ]

-- | Build a 'Spec' that contains one property with the given name, which
-- contains the given streams, and is defined by the given boolean expression,
-- which is universally quantified.
forallPropSpec :: String -> [Stream] -> Expr Bool -> Spec
forallPropSpec propName propStreams propExpr =
  Spec propStreams [] [] [Copilot.Property propName (Copilot.Forall propExpr)]

-- | Build a 'Spec' that contains one property with the given name, which
-- contains the given streams, and is defined by the given boolean expression,
-- which is existentially quantified.
existsPropSpec :: String -> [Stream] -> Expr Bool -> Spec
existsPropSpec propName propStreams propExpr =
  Spec propStreams [] [] [Copilot.Property propName (Copilot.Exists propExpr)]
