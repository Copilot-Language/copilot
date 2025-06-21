{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This will fail to verify since the verification does not assume the
-- @withinBounds@ property, which is needed to prevent an out-of-bounds array index.
module Copilot.Verifier.Examples.ShouldFail.Partial.IndexOutOfBounds where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )

spec :: Spec
spec = do
  let stream1 :: Stream (Array 2 Int16)
      stream1 = constant (array [27, 42])

      stream2 :: Stream Word32
      stream2 = extern "stream2" Nothing

  _ <- prop "withinBounds" (forAll (stream2 < constW32 2))
  trigger "streamIndex" ((stream1 ! stream2) == 1) [arg stream1, arg stream2]

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions defaultVerifierOptions{verbosity = verb}
    mkDefaultCSettings
    -- ["withinBounds"]
    []
    "indexFail" spec'
