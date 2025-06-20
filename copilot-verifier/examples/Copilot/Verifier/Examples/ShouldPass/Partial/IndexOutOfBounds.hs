{-# LANGUAGE NoImplicitPrelude #-}

-- | This will succeed with 'sideCondVerifierOptions', as Copilot's indexing
-- operation should be out of bounds precisely when C array indexes are out of bounds.
module Copilot.Verifier.Examples.ShouldPass.Partial.IndexOutOfBounds where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , sideCondVerifierOptions, verifyWithOptions )
import Copilot.Verifier.Examples.ShouldFail.Partial.IndexOutOfBounds (spec)

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions sideCondVerifierOptions{verbosity = verb}
    mkDefaultCSettings
    []
    "indexPass" spec'
