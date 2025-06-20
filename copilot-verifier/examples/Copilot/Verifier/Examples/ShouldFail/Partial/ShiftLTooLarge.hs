{-# LANGUAGE NoImplicitPrelude #-}

-- | This will fail to verify since the verification does not assume the
-- @lessThanBitWidth@ property, which is needed to prevent shifting by too
-- large of a value.
module Copilot.Verifier.Examples.ShouldFail.Partial.ShiftLTooLarge where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )

spec :: Spec
spec = do
  let stream1 :: Stream Int32
      stream1 = extern "stream1" Nothing

      stream2 :: Stream Int64
      stream2 = extern "stream2" Nothing

  _ <- prop "lessThanBitWidth" (forAll
         (constI64 0 <= stream2 && stream2 < constI64 32))
  trigger "streamShiftL" ((stream1 .<<. stream2) == 1) [arg stream1, arg stream2]

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions defaultVerifierOptions{verbosity = verb}
    mkDefaultCSettings
    -- ["lessThanBitWidth"]
    []
    "shiftLTooLargeFail" spec'
