{-# LANGUAGE NoImplicitPrelude #-}

-- | This will succeed with 'sideCondVerifierOptions', as Copilot's @BwShiftR@
-- operation should only accept second arguments as large as C's @>>@ operation does.
module Copilot.Verifier.Examples.ShouldPass.Partial.ShiftRTooLarge where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , sideCondVerifierOptions, verifyWithOptions )
import Copilot.Verifier.Examples.ShouldFail.Partial.ShiftRTooLarge (spec)

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions sideCondVerifierOptions{verbosity = verb}
    mkDefaultCSettings
    []
    "shiftRTooLargePass" spec'
