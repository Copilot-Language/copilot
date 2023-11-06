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
import Copilot.Core.Type.Array (Array, array, arrayElems)

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
    ]

-- * Individual tests

-- | Test that building an array from a list and extracting the elements with
-- the function 'arrayElems' will result in the same list.
testArrayElemsLeft :: forall n . KnownNat n => Proxy n -> Property
testArrayElemsLeft len =
    forAll xsInt64 $ \ls ->
      let array' :: Array n Int64
          array' = array ls
      in arrayElems array' == ls

  where

    -- Generator for lists of Int64 of known length.
    xsInt64 :: Gen [Int64]
    xsInt64 = vectorOf (fromIntegral (natVal len)) arbitrary
