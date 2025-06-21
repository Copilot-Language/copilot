{-# LANGUAGE NoImplicitPrelude #-}

-- | This will succeed with 'sideCondVerifierOptions', as Copilot's @Div@
-- operation is well defined precisely when C division is well defined.
module Copilot.Verifier.Examples.ShouldPass.Partial.DivByZero where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , sideCondVerifierOptions, verifyWithOptions )
import Copilot.Verifier.Examples.ShouldFail.Partial.DivByZero (spec)

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions sideCondVerifierOptions{verbosity = verb}
    mkDefaultCSettings
    []
    "divByZeroPass" spec'
