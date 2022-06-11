-- The following warning is enabled in this module so that the import of
-- Copilot.Core.External does not give rise to a warning.
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | Test copilot-core:Copilot.Core.External.
module Test.Copilot.Core.External where

-- External imports
import Data.List                      (sort)
import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (assertBool)

-- Internal imports: library modules being tested
import Copilot.Core.Expr      (Expr (ExternVar, Op1))
import Copilot.Core.External  (externVarName, externVars)
import Copilot.Core.Operators (Op1 (Abs, Sign))
import Copilot.Core.Spec      (Spec (..), Stream (..))
import Copilot.Core.Type      (Type (Int64))

-- | All unit tests for copilot-core:Copilot.Core.External.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Core.External"
    [ testCase "externVars" testExternVarsFind
    ]

-- * Individual tests

-- | Test that 'externVars' will find an extern in a spec.
testExternVarsFind :: IO ()
testExternVarsFind = do
  -- A simple stream and a nested stream
  let s1     = Stream 1 [] (ExternVar Int64 "z" Nothing) Int64
      s2     = Stream 2 [] s2Expr Int64
      s2Expr = Op1 (Abs Int64)
             $ Op1 (Sign Int64)
             $ ExternVar Int64 "y" Nothing

  -- Calculate the expected external vars in a spec
  let res = sort $ map externVarName $ externVars $ Spec [s1, s2] [] [] []

  -- Compare result with the expectation
  let success = [ "y", "z" ] == res
  assertBool
    "The function externVars could not find the expected externs"
    success
