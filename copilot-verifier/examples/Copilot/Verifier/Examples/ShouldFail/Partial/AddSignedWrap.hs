{-# LANGUAGE NoImplicitPrelude #-}

-- | This will fail to verify since the verification does not assume the
-- @notIntMax@ property, which is needed to prevent signed integer overflow.
module Copilot.Verifier.Examples.ShouldFail.Partial.AddSignedWrap where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )

spec :: Spec
spec = do
  let stream :: Stream Int32
      stream = extern "stream" Nothing

  _ <- prop "notIntMax" (forAll (stream < constI32 maxBound))
  trigger "streamAddSigned" ((stream + 1) == 1) [arg stream]

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions defaultVerifierOptions{verbosity = verb}
    mkDefaultCSettings
    -- ["notIntMax"]
    []
    "addSignedWrapFail" spec'
