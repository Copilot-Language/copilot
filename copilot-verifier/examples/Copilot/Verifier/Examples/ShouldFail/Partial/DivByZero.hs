{-# LANGUAGE NoImplicitPrelude #-}

-- | This will fail to verify since the verification does not assume the
-- @nonzero@ property, which is needed to prevent a division-by-zero error.
module Copilot.Verifier.Examples.ShouldFail.Partial.DivByZero where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )

spec :: Spec
spec = do
  let stream :: Stream Int16
      stream = extern "stream" Nothing

  _ <- prop "nonzero" (forAll (stream /= 0))
  trigger "streamDiv" ((stream `div` stream) == 1) [arg stream]

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions defaultVerifierOptions{verbosity = verb}
    mkDefaultCSettings
    -- ["nonzero"]
    []
    "divByZeroFail" spec'
