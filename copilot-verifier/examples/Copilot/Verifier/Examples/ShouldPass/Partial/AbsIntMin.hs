{-# LANGUAGE NoImplicitPrelude #-}

-- | This will succeed with 'sideCondVerifierOptions', as Copilot's @Abs@
-- operation is well defined precisely when C's @abs@ function is well defined.
module Copilot.Verifier.Examples.ShouldPass.Partial.AbsIntMin where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , sideCondVerifierOptions, verifyWithOptions )
import Copilot.Verifier.Examples.ShouldFail.Partial.AbsIntMin (spec)

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions sideCondVerifierOptions{verbosity = verb}
    mkDefaultCSettings
    []
    "absIntMinPass" spec'
