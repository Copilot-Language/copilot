{-# LANGUAGE OverloadedStrings #-}
module Copilot.Verifier.Examples
  ( shouldFailExamples
  , shouldPassExamples
  ) where

import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)

import Copilot.Verifier (Verbosity)
import qualified Copilot.Verifier.Examples.ShouldFail.Partial.AbsIntMin        as Fail.AbsIntMin
import qualified Copilot.Verifier.Examples.ShouldFail.Partial.AddSignedWrap    as Fail.AddSignedWrap
import qualified Copilot.Verifier.Examples.ShouldFail.Partial.DivByZero        as Fail.DivByZero
import qualified Copilot.Verifier.Examples.ShouldFail.Partial.IndexOutOfBounds as Fail.IndexOutOfBounds
import qualified Copilot.Verifier.Examples.ShouldFail.Partial.ModByZero        as Fail.ModByZero
import qualified Copilot.Verifier.Examples.ShouldFail.Partial.MulSignedWrap    as Fail.MulSignedWrap
import qualified Copilot.Verifier.Examples.ShouldFail.Partial.ShiftLTooLarge   as Fail.ShiftLTooLarge
import qualified Copilot.Verifier.Examples.ShouldFail.Partial.ShiftRTooLarge   as Fail.ShiftRTooLarge
import qualified Copilot.Verifier.Examples.ShouldFail.Partial.SubSignedWrap    as Fail.SubSignedWrap
import qualified Copilot.Verifier.Examples.ShouldPass.Array                    as Array
import qualified Copilot.Verifier.Examples.ShouldPass.ArrayGen                 as ArrayGen
import qualified Copilot.Verifier.Examples.ShouldPass.ArrayOfStructs           as ArrayOfStructs
import qualified Copilot.Verifier.Examples.ShouldPass.ArrayTriggerArgument     as ArrayTriggerArgument
import qualified Copilot.Verifier.Examples.ShouldPass.Arith                    as Arith
import qualified Copilot.Verifier.Examples.ShouldPass.Clock                    as Clock
import qualified Copilot.Verifier.Examples.ShouldPass.Counter                  as Counter
import qualified Copilot.Verifier.Examples.ShouldPass.Engine                   as Engine
import qualified Copilot.Verifier.Examples.ShouldPass.FPNegation               as FPNegation
import qualified Copilot.Verifier.Examples.ShouldPass.FPOps                    as FPOps
import qualified Copilot.Verifier.Examples.ShouldPass.Heater                   as Heater
import qualified Copilot.Verifier.Examples.ShouldPass.IntOps                   as IntOps
import qualified Copilot.Verifier.Examples.ShouldPass.Partial.AbsIntMin        as Pass.AbsIntMin
import qualified Copilot.Verifier.Examples.ShouldPass.Partial.AddSignedWrap    as Pass.AddSignedWrap
import qualified Copilot.Verifier.Examples.ShouldPass.Partial.IndexOutOfBounds as Pass.IndexOutOfBounds
import qualified Copilot.Verifier.Examples.ShouldPass.Partial.DivByZero        as Pass.DivByZero
import qualified Copilot.Verifier.Examples.ShouldPass.Partial.ModByZero        as Pass.ModByZero
import qualified Copilot.Verifier.Examples.ShouldPass.Partial.MulSignedWrap    as Pass.MulSignedWrap
import qualified Copilot.Verifier.Examples.ShouldPass.Partial.ShiftLTooLarge   as Pass.ShiftLTooLarge
import qualified Copilot.Verifier.Examples.ShouldPass.Partial.ShiftRTooLarge   as Pass.ShiftRTooLarge
import qualified Copilot.Verifier.Examples.ShouldPass.Partial.SubSignedWrap    as Pass.SubSignedWrap
import qualified Copilot.Verifier.Examples.ShouldPass.Structs                  as Structs
import qualified Copilot.Verifier.Examples.ShouldPass.UpdateArray              as UpdateArray
import qualified Copilot.Verifier.Examples.ShouldPass.UpdateStruct             as UpdateStruct
import qualified Copilot.Verifier.Examples.ShouldPass.Voting                   as Voting
import qualified Copilot.Verifier.Examples.ShouldPass.WCV                      as WCV

shouldFailExamples :: Verbosity -> Map (CI Text) (IO ())
shouldFailExamples verb = Map.fromList
    [ -- Partial operation tests
      example "AbsIntMin-fail" (Fail.AbsIntMin.verifySpec verb)
    , example "AddSignedWrap-fail" (Fail.AddSignedWrap.verifySpec verb)
    , example "DivByZero-fail" (Fail.DivByZero.verifySpec verb)
    , example "IndexOutOfBounds-fail" (Fail.IndexOutOfBounds.verifySpec verb)
    , example "ModByZero-fail" (Fail.ModByZero.verifySpec verb)
    , example "MulSignedWrap-fail" (Fail.MulSignedWrap.verifySpec verb)
    , example "ShiftLTooLarge-fail" (Fail.ShiftLTooLarge.verifySpec verb)
    , example "ShiftRTooLarge-fail" (Fail.ShiftRTooLarge.verifySpec verb)
    , example "SubSignedWrap-fail" (Fail.SubSignedWrap.verifySpec verb)
    ]

shouldPassExamples :: Verbosity -> Map (CI Text) (IO ())
shouldPassExamples verb = Map.fromList
    [ example "Array" (Array.verifySpec verb)
    , example "ArrayGen" (ArrayGen.verifySpec verb)
    , example "ArrayOfStructs" (ArrayOfStructs.verifySpec verb)
    , example "ArrayTriggerArgument" (ArrayTriggerArgument.verifySpec verb)
    , example "Arith" (Arith.verifySpec verb)
    , example "Clock" (Clock.verifySpec verb)
    , example "Counter" (Counter.verifySpec verb)
    , example "Engine" (Engine.verifySpec verb)
    , example "FPNegation" (FPNegation.verifySpec verb)
    , example "FPOps" (FPOps.verifySpec verb)
    , example "Heater" (Heater.verifySpec verb)
    , example "IntOps" (IntOps.verifySpec verb)
    , example "Structs" (Structs.verifySpec verb)
    , example "UpdateArray" (UpdateArray.verifySpec verb)
    , example "UpdateStruct" (UpdateStruct.verifySpec verb)
    , example "Voting" (Voting.verifySpec verb)
    , example "WCV" (WCV.verifySpec verb)

      -- Partial operation tests
    , example "AbsIntMin-pass" (Pass.AbsIntMin.verifySpec verb)
    , example "AddSignedWrap-pass" (Pass.AddSignedWrap.verifySpec verb)
    , example "DivByZero-pass" (Pass.DivByZero.verifySpec verb)
    , example "IndexOutOfBounds-pass" (Pass.IndexOutOfBounds.verifySpec verb)
    , example "ModByZero-pass" (Pass.ModByZero.verifySpec verb)
    , example "MulSignedWrap-pass" (Pass.MulSignedWrap.verifySpec verb)
    , example "ShiftLTooLarge-pass" (Pass.ShiftLTooLarge.verifySpec verb)
    , example "ShiftRTooLarge-pass" (Pass.ShiftRTooLarge.verifySpec verb)
    , example "SubSignedWrap-pass" (Pass.SubSignedWrap.verifySpec verb)
    ]

example :: Text -> IO () -> (CI Text, IO ())
example name action = (CI.mk name, action)
