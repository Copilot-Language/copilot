-- | Test copilot-core:Copilot.Core.Type.
module Test.Copilot.Core.Type where

-- External imports
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Property, elements, forAllBlind,
                                             shuffle, (==>))

-- Internal imports: library modules being tested
import Copilot.Core.Type (SimpleType (..))

-- | All unit tests for copilot-core:Copilot.Core.Type.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Core.Type"
    [ testProperty "reflexivity of equality of simple types"
        testSimpleTypesEqualityReflexive
    , testProperty "symmetry of equality of simple types"
        testSimpleTypesEqualitySymmetric
    , testProperty "transitivity of equality of simple types"
        testSimpleTypesEqualityTransitive
    , testProperty "uniqueness of equality of simple types"
        testSimpleTypesEqualityUniqueness
    ]

-- | Test that the equality relation for simple types is reflexive.
testSimpleTypesEqualityReflexive :: Property
testSimpleTypesEqualityReflexive =
  forAllBlind (elements simpleTypes) $ \t ->
    t == t

-- | Test that the equality relation for simple types is symmetric.
testSimpleTypesEqualitySymmetric :: Property
testSimpleTypesEqualitySymmetric =
  forAllBlind (elements simpleTypes) $ \t1 ->
  forAllBlind (elements simpleTypes) $ \t2 ->
    t1 == t2 ==> t2 == t1

-- | Test that the equality relation for simple types is transitive.
testSimpleTypesEqualityTransitive :: Property
testSimpleTypesEqualityTransitive =
  forAllBlind (elements simpleTypes) $ \t1 ->
  forAllBlind (elements simpleTypes) $ \t2 ->
  forAllBlind (elements simpleTypes) $ \t3 ->
    (t1 == t2 && t2 == t3) ==> (t1 == t3)

-- | Test that each type is only equal to itself.
testSimpleTypesEqualityUniqueness :: Property
testSimpleTypesEqualityUniqueness =
  forAllBlind (shuffle simpleTypes) $ \(t:ts) ->
    notElem t ts

-- | Simple types tested.
simpleTypes :: [SimpleType]
simpleTypes =
  [ SBool
  , SInt8
  , SInt16
  , SInt32
  , SInt64
  , SWord8
  , SWord16
  , SWord32
  , SWord64
  , SFloat
  , SDouble
  , SStruct
  ]
