{-# LANGUAGE DataKinds #-}
-- The following warning is disabled due to a necessary instance of SatResult
-- defined in this module.
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Test copilot-theorem:Copilot.Theorem.What4.
module Test.Copilot.Theorem.What4 where

-- External imports
import Data.Int                             (Int8)
import Data.Word                            (Word32)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Arbitrary (arbitrary), Property,
                                             arbitrary, forAll)
import Test.QuickCheck.Monadic              (monadicIO, run)

-- External imports: Copilot
import           Copilot.Core.Expr      (Expr (Const, Op1, Op2))
import           Copilot.Core.Operators (Op1 (..), Op2 (..))
import           Copilot.Core.Spec      (Spec (..))
import qualified Copilot.Core.Spec      as Copilot
import           Copilot.Core.Type      (Field (..),
                                         Struct (toValues, typeName),
                                         Type (Struct), Typed (typeOf),
                                         Value (..))

-- Internal imports: Modules being tested
import Copilot.Theorem.What4 (SatResult (..), Solver (..), prove)

-- * Constants

-- | Unit tests for copilot-theorem:Copilot.Theorem.What4.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Theorem.What4"
    [ testProperty "Prove via Z3 that true is valid"    testProveZ3True
    , testProperty "Prove via Z3 that false is invalid" testProveZ3False
    , testProperty "Prove via Z3 that x == x is valid"  testProveZ3EqConst
    , testProperty "Prove via Z3 that a struct update is valid" testProveZ3StructUpdate
    ]

-- * Individual tests

-- | Test that Z3 is able to prove the following expression valid:
-- @
--   constant True
-- @
testProveZ3True :: Property
testProveZ3True =
    monadicIO $ run $ checkResult Z3 propName spec Valid
  where
    propName :: String
    propName = "prop"

    spec :: Spec
    spec = propSpec propName $ Const typeOf True

-- | Test that Z3 is able to prove the following expression invalid:
-- @
--   constant False
-- @
testProveZ3False :: Property
testProveZ3False =
    monadicIO $ run $ checkResult Z3 propName spec Invalid
  where
    propName :: String
    propName = "prop"

    spec :: Spec
    spec = propSpec propName $ Const typeOf False

-- | Test that Z3 is able to prove the following expression valid:
-- @
--   for all (x :: Int8), constant x == constant x
-- @
testProveZ3EqConst :: Property
testProveZ3EqConst = forAll arbitrary $ \x ->
    monadicIO $ run $ checkResult Z3 propName (spec x) Valid
  where
    propName :: String
    propName = "prop"

    spec :: Int8 -> Spec
    spec x = propSpec propName $
      Op2 (Eq typeOf) (Const typeOf x) (Const typeOf x)

-- | Test that Z3 is able to prove the following expression valid:
-- @
--   for all (s :: MyStruct),
--   ((s ## testField =$ (+1)) # testField) == ((s # testField) + 1)
-- @
testProveZ3StructUpdate :: Property
testProveZ3StructUpdate = forAll arbitrary $ \x ->
    monadicIO $ run $ checkResult Z3 propName (spec x) Valid
  where
    propName :: String
    propName = "prop"

    spec :: TestStruct -> Spec
    spec s = propSpec propName $
      Op2
        (Eq typeOf)
        (getField
          (Op2
            (UpdateField typeOf typeOf testField)
            sExpr
            (add1 (getField sExpr))))
        (add1 (getField sExpr))
      where
        sExpr :: Expr TestStruct
        sExpr = Const typeOf s

        getField :: Expr TestStruct -> Expr Word32
        getField = Op1 (GetField typeOf typeOf testField)

        add1 :: Expr Word32 -> Expr Word32
        add1 x = Op2 (Add typeOf) x (Const typeOf 1)

-- | A simple data type with a 'Struct' instance and a 'Field'. This is only
-- used as part of 'testProveZ3StructUpdate'.
newtype TestStruct = TestStruct { testField :: Field "testField" Word32 }

instance Arbitrary TestStruct where
  arbitrary = do
    w32 <- arbitrary
    return (TestStruct (Field w32))

instance Struct TestStruct where
  typeName _ = "testStruct"
  toValues s = [Value typeOf (testField s)]

instance Typed TestStruct where
  typeOf = Struct (TestStruct (Field 0))

-- | Check that the solver's satisfiability result for the given property in
-- the given spec matches the expectation.
checkResult :: Solver -> String -> Spec -> SatResult -> IO Bool
checkResult solver propName spec expectation = do
  results <- prove solver spec

  -- Find the satisfiability result for propName.
  let propResult = lookup propName results

  -- The following check also works for the case in which the property name
  -- does not exist in the results, in which case the lookup returns 'Nothing'.
  return $ propResult == Just expectation

-- * Auxiliary

-- | Build a 'Spec' that contains one property with the given name and defined
-- by the given boolean expression.
propSpec :: String -> Expr Bool -> Spec
propSpec propName propExpr = Spec [] [] [] [Copilot.Property propName propExpr]

-- | Equality for 'SatResult'.
--
-- This is an orphan instance, so we suppress the warning that GHC would
-- normally produce with a GHC option at the top.
instance Eq SatResult where
  Valid   == Valid   = True
  Invalid == Invalid = True
  Unknown == Unknown = True
  _       == _       = False
