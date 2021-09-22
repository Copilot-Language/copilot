{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Test copilot-core:Copilot.Core.Type.Array.
module Test.Copilot.Core.Type.Array where

-- External imports
import Data.Int                             (Int64)
import Data.Proxy                           (Proxy (..))
import GHC.TypeNats                         (KnownNat, natVal)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Gen, Property, arbitrary, forAll,
                                             vectorOf)

-- Internal imports: library modules being tested
import Copilot.Core.Type.Array (Array, array, arrayelems, flatten, size)

-- | All unit tests for copilot-core:Copilot.Core.Array.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Core.Type.Array"
    [ testProperty "arrayElems . array (identity; 0)"
        (testArrayElemsLeft (Proxy :: Proxy 0))
    , testProperty "arrayElems . array (identity; 5)"
        (testArrayElemsLeft (Proxy :: Proxy 5))
    , testProperty "arrayElems . array (identity; 200)"
        (testArrayElemsLeft (Proxy :: Proxy 200))
    , testProperty "arrayFlatten . array (identity; 0)"
        (testArrayFlattenLeft (Proxy :: Proxy 0))
    , testProperty "arrayFlatten . array (identity; 1)"
        (testArrayFlattenLeft (Proxy :: Proxy 1))
    , testProperty "arrayFlatten . array (identity; 2)"
        (testArrayFlattenLeft (Proxy :: Proxy 2))
    , testProperty "arrayFlatten . array (identity; 22)"
        (testArrayFlattenLeft (Proxy :: Proxy 22))
    , testProperty "arrayFlatten . array (identity; 50)"
        (testArrayFlattenLeft (Proxy :: Proxy 50))
    , testProperty "arrayFlatten . array (identity; nested; nested; 2x6)"
        (testArrayFlattenNestedLeft (Proxy :: Proxy 2) (Proxy :: Proxy 6))
    , testProperty "arrayFlatten . array (identity; nested; nested; 10x2)"
        (testArrayFlattenNestedLeft (Proxy :: Proxy 10) (Proxy :: Proxy 2))
    , testProperty "arrayFlatten . array (identity; nested; nested; 38x20)"
        (testArrayFlattenNestedLeft (Proxy :: Proxy 38) (Proxy :: Proxy 20))
    , testProperty "arrayFlatten . array (identity; nested; nested; 125x100)"
        (testArrayFlattenNestedLeft (Proxy :: Proxy 125) (Proxy :: Proxy 100))
    , testProperty "arraySize . array (identity; 0)"
        (testArraySizeLeft (Proxy :: Proxy 0))
    , testProperty "arraySize . array (identity; 45)"
        (testArraySizeLeft (Proxy :: Proxy 45))
    , testProperty "arraySize . array (identity; 100)"
        (testArraySizeLeft (Proxy :: Proxy 100))
    ]

-- * Individual tests

-- | Test that building an array from a list and extracting the elements with
-- the function 'arrayelems' will result in the same list.
testArrayElemsLeft :: forall n . KnownNat n => Proxy n -> Property
testArrayElemsLeft len =
    forAll xsInt64 $ \ls ->
      let array' :: Array n Int64
          array' = array ls
      in arrayelems array' == ls

  where

    -- Generator for lists of Int64 of known length.
    xsInt64 :: Gen [Int64]
    xsInt64 = vectorOf (fromIntegral (natVal len)) arbitrary

-- | Test that building an array from a list and extracting the elements with
-- the function 'flatten' will result in the same list.
--
-- This test tests only plain arrays (no nesting).
testArrayFlattenLeft :: forall n . KnownNat n => Proxy n -> Property
testArrayFlattenLeft len =
    forAll xsInt64 $ \ls ->
      let array' :: Array n Int64
          array' = array ls
      in flatten array' == ls

  where

    -- Generator for lists of Int64 of known length.
    xsInt64 :: Gen [Int64]
    xsInt64 = vectorOf (fromIntegral (natVal len)) arbitrary

-- | Test that building a nested array of a known size and extracting the
-- elements with the function 'flatten' will result in a list with a size that
-- is the product of the sizes provided.
testArrayFlattenNestedLeft :: forall n1 n2
                           .  (KnownNat n1, KnownNat n2)
                           => Proxy n1
                           -> Proxy n2
                           -> Property
testArrayFlattenNestedLeft len1 len2 =
    forAll xsArrays $ \array' ->
      length (flatten array' :: [Int64]) == expectedSize

  where

    -- Expected size of the matrix / array.
    expectedSize :: Int
    expectedSize = fromIntegral $ natVal len1 * natVal len2

    -- Generator for nested matrices of Int64 of known size.
    xsArrays :: Gen (Array n1 (Array n2 Int64))
    xsArrays =
      array <$> vectorOf (fromIntegral (natVal len1)) xsArrayInt64

    -- Generator for arrays of Int64 of known length.
    xsArrayInt64 :: Gen (Array n2 Int64)
    xsArrayInt64 = array <$> xsInt64

    -- Generator for lists of Int64 of known length.
    xsInt64 :: Gen [Int64]
    xsInt64 = vectorOf (fromIntegral (natVal len2)) arbitrary

-- | Test that building an array from a list and calculating the size will
-- return the length of the list.
testArraySizeLeft :: forall n . KnownNat n => Proxy n -> Property
testArraySizeLeft len =
    forAll xsInt64 $ \ls ->
      let array' :: Array n Int64
          array' = array ls
      in size array' == length ls

  where

    -- Generator for lists of Int64 of known length.
    xsInt64 :: Gen [Int64]
    xsInt64 = vectorOf (fromIntegral (natVal len)) arbitrary
