{-# LANGUAGE GADTs #-}
-- | Test copilot-core:Copilot.Core.Type.Dynamic.
module Test.Copilot.Core.Type.Dynamic where

-- External imports
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Arbitrary, Property, property)

-- Internal imports: library modules being tested
import Copilot.Core.Type          (Type (..))
import Copilot.Core.Type.Dynamic  (fromDyn, toDyn)
import Copilot.Core.Type.Equality (Equal (Refl), EqualType ((=~=)))

-- | All unit tests for copilot-core:Copilot.Core.Type.Dynamic.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Core.Type.Dynamic"
    [ testProperty "fromDyn . toDyn == identity (Int64)"
        (testDynamicIdentity Int64 Int64)
    , testProperty "fromDyn . toDyn == identity (Bool)"
        (testDynamicIdentity Bool Bool)
    , testProperty "fromDyn . toDyn == identity (Float)"
        (testDynamicIdentity Float Float)
    , testProperty "fromDyn . toDyn == identity (Double)"
        (testDynamicIdentity Double Double)
    , testProperty "fromDyn . toDyn /= identity (Int64 vs Int32)"
        (testDynamicIdentity Int64 Int32)
    , testProperty "fromDyn . toDyn /= identity (Int64 vs Int16)"
        (testDynamicIdentity Int64 Int16)
    , testProperty "fromDyn . toDyn /= identity (Bool vs Int16)"
        (testDynamicIdentity Bool Int16)
    , testProperty "fromDyn . toDyn /= identity (Float vs Double)"
        (testDynamicIdentity Float Double)
    ]

-- | Test that wrapping in and unwrapping from a dynamic gets the original
-- value if the types match, and Nothing otherwise.
testDynamicIdentity :: (Eq a, Arbitrary b)
                    => Type a  -- ^ Type to unwrap to
                    -> Type b  -- ^ Type to wrap to
                    -> b       -- ^ Value to wrap
                    -> Property
testDynamicIdentity t1 t2 ls = property $
  let unwrapped = fromDyn t1 (toDyn t2 ls)
  in case t1 =~= t2 of
       Nothing   -> Nothing == unwrapped
       Just Refl -> Just ls == unwrapped
