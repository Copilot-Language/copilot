{-# LANGUAGE NoImplicitPrelude #-}
module Copilot.Verifier.Examples.ShouldPass.IntOps where

import Copilot.Compile.C99 (mkDefaultCSettings)
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )
import Language.Copilot
import qualified Prelude as P

spec :: Spec
spec = do
  let stream :: Stream Int16
      stream = extern "stream" Nothing

      shiftBy :: Stream Int16
      shiftBy = extern "shiftBy" Nothing

  _ <- prop "nonzero" (forAll (stream /= 0))
  _ <- prop "shiftByBits" (forAll (0 <= shiftBy && shiftBy < 16))

  triggerOp1 "abs" abs stream
  triggerOp1 "signum" signum stream
  triggerOp1 "bwNot" complement stream

  triggerOp2 "add" (+) stream stream
  triggerOp2 "sub" (-) stream stream
  triggerOp2 "mul" (*) stream stream
  triggerOp2 "mod" mod stream stream
  triggerOp2 "div" div stream stream
  triggerOp2 "bwAnd" (.&.) stream stream
  triggerOp2 "bwOr" (.|.) stream stream
  triggerOp2 "bwXor" (.^.) stream stream
  triggerOp2 "bwShiftL" (.<<.) stream shiftBy
  triggerOp2 "bwShiftR" (.>>.) stream shiftBy

triggerOp1 :: String ->
              (Stream Int16 -> Stream Int16) ->
              Stream Int16 ->
              Spec
triggerOp1 name op stream =
  trigger (name P.++ "Trigger") (testOp1 op stream) [arg stream]

triggerOp2 :: String ->
              (Stream Int16 -> Stream Int16 -> Stream Int16) ->
              Stream Int16 -> Stream Int16 ->
              Spec
triggerOp2 name op stream1 stream2 =
  trigger (name P.++ "Trigger") (testOp2 op stream1 stream2) [arg stream1, arg stream2]

testOp1 :: (Stream Int16 -> Stream Int16) -> Stream Int16 -> Stream Bool
testOp1 op stream =
  op stream == op stream

testOp2 :: (Stream Int16 -> Stream Int16 -> Stream Int16) ->
           Stream Int16 -> Stream Int16 ->
           Stream Bool
testOp2 op stream1 stream2 =
  op stream1 stream2 == op stream1 stream2

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions defaultVerifierOptions{verbosity = verb}
                    mkDefaultCSettings ["nonzero", "shiftByBits"] "intOps" spec'
