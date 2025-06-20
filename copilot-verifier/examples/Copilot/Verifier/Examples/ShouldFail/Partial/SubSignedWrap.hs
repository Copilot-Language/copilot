{-# LANGUAGE NoImplicitPrelude #-}

-- | This will fail to verify since the verification does not assume the
-- @notIntMin@ property, which is needed to prevent signed integer underflow.
module Copilot.Verifier.Examples.ShouldFail.Partial.SubSignedWrap where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )

spec :: Spec
spec = do
  let stream :: Stream Int32
      stream = extern "stream" Nothing

  _ <- prop "notIntMin" (forAll (stream > constI32 minBound))
  trigger "streamSubSigned" ((stream - 1) == 1) [arg stream]

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions defaultVerifierOptions{verbosity = verb}
    mkDefaultCSettings
    -- ["notIntMin"]
    []
    "subSignedWrapFail" spec'
