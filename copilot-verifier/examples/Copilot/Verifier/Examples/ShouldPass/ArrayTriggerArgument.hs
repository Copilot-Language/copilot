{-# LANGUAGE DataKinds #-}

-- | A regression test for
-- <https://github.com/Copilot-Language/copilot/issues/431>.
module Copilot.Verifier.Examples.ShouldPass.ArrayTriggerArgument where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )

spec :: Spec
spec = trigger "f" true [arg stream]
  where
    stream :: Stream (Array 2 Int16)
    stream = constant (array [3,4])

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions
    defaultVerifierOptions{verbosity = verb}
    mkDefaultCSettings [] "arrayTriggerArgument" spec'
