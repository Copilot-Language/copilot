{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Copilot.Verifier.Examples.ShouldPass.FPOps where

import Copilot.Compile.C99 (mkDefaultCSettings)
import qualified Copilot.Language.Stream as Copilot
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )
import Data.Proxy (Proxy(..))
import Language.Copilot
import qualified Prelude as P

mkSpecFor :: forall a proxy. (RealFloat a, Typed a) => proxy a -> String -> Spec
mkSpecFor _ nameSuffix = do
  let mkName :: String -> String
      mkName namePrefix = namePrefix P.++ nameSuffix

      stream :: Stream a
      stream = extern (mkName "stream") Nothing

  triggerOp1 (mkName "abs") abs stream
  triggerOp1 (mkName "signum") signum stream
  triggerOp1 (mkName "recip") recip stream
  triggerOp1 (mkName "exp") exp stream
  triggerOp1 (mkName "sqrt") sqrt stream
  triggerOp1 (mkName "log") log stream
  triggerOp1 (mkName "sin") sin stream
  triggerOp1 (mkName "tan") tan stream
  triggerOp1 (mkName "cos") cos stream
  triggerOp1 (mkName "asin") asin stream
  triggerOp1 (mkName "atan") atan stream
  triggerOp1 (mkName "acos") acos stream
  triggerOp1 (mkName "sinh") sinh stream
  triggerOp1 (mkName "tanh") tanh stream
  triggerOp1 (mkName "cosh") cosh stream
  triggerOp1 (mkName "asinh") asinh stream
  triggerOp1 (mkName "atanh") atanh stream
  triggerOp1 (mkName "acosh") acosh stream
  triggerOp1 (mkName "ceiling") Copilot.ceiling stream
  triggerOp1 (mkName "floor") Copilot.floor stream

  triggerOp2 (mkName "add") (+) stream
  triggerOp2 (mkName "sub") (-) stream
  triggerOp2 (mkName "mul") (*) stream
  triggerOp2 (mkName "div") (/) stream
  triggerOp2 (mkName "pow") (**) stream
  triggerOp2 (mkName "logBase") logBase stream
  triggerOp2 (mkName "atan2") Copilot.atan2 stream

spec :: Spec
spec = do
  mkSpecFor (Proxy @Float) "F"
  mkSpecFor (Proxy @Double) "D"

triggerOp1 :: (RealFloat a, Typed a) =>
              String ->
              (Stream a -> Stream a) ->
              Stream a ->
              Spec
triggerOp1 name op stream =
  trigger (name P.++ "Trigger") (testOp1 op stream) [arg stream]

triggerOp2 :: (RealFloat a, Typed a) =>
              String ->
              (Stream a -> Stream a -> Stream a) ->
              Stream a ->
              Spec
triggerOp2 name op stream =
  trigger (name P.++ "Trigger") (testOp2 op stream) [arg stream]

testOp1 :: (RealFloat a, Typed a) =>
           (Stream a -> Stream a) -> Stream a -> Stream Bool
testOp1 op stream =
  -- NB: Use (>=) rather than (==) here, as floating-point equality gets weird
  -- due to NaNs.
  op stream >= op stream

testOp2 :: (RealFloat a, Typed a) =>
           (Stream a -> Stream a -> Stream a) -> Stream a -> Stream Bool
testOp2 op stream =
  op stream stream >= op stream stream

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions defaultVerifierOptions{verbosity = verb}
                    mkDefaultCSettings [] "fpOps" spec'
