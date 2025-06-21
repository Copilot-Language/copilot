{-# LANGUAGE NoImplicitPrelude #-}

-- | This will fail to verify since the verification does not assume the
-- @withinRange@ property, which is needed to prevent signed integer underflow or overflow.
module Copilot.Verifier.Examples.ShouldFail.Partial.MulSignedWrap where

import qualified Prelude as P

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )

spec :: Spec
spec = do
  let stream :: Stream Int32
      stream = extern "stream" Nothing

  _ <- prop "withinRange" (forAll
           (constI32 ((minBound P.+ 1) P.* 2) < stream
         && stream < constI32 ((maxBound P.- 1) `P.div` 2)))
  trigger "streamMulSigned" ((stream * 2) == 2) [arg stream]

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions defaultVerifierOptions{verbosity = verb}
    mkDefaultCSettings
    -- ["withinRange"]
    []
    "mulSignedWrapFail" spec'
