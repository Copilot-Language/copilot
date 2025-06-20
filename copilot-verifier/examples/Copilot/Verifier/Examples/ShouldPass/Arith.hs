
{-# LANGUAGE RebindableSyntax #-}

module Copilot.Verifier.Examples.ShouldPass.Arith where

import Control.Monad (when)
import qualified Prelude as P

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity(..), VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )
import Copilot.Theorem.What4 (prove, Solver(..))

-- The largest unsigned 32-bit prime
lastPrime :: Stream Word32
lastPrime = 31
--lastPrime = 4294967291 -- Whelp, this prime seems too big for the solvers to handle well

multRingSpec :: Spec
multRingSpec = do
  _ <- prop "clamp nonzero" (forAll ((clamp > 0) && (clamp < lastPrime)))
  _ <- prop "reduced" (forAll (acc < lastPrime))
  _ <- prop "nonzero" (forAll (acc > 0 && (acc < lastPrime)))

  trigger "outofrange" (not (acc > 0 && acc < lastPrime)) [arg acc]

  return ()

  where
  -- a stream of external values
  vals  = externW32 "values" Nothing

  -- Generate a value in [1, lastPrime), which
  -- is the multiplictive group of Z_p
  clamp :: Stream Word32
  clamp = (vals `mod` (lastPrime - 1)) + 1

  -- Successively multiply new values
  acc :: Stream Word32
  acc = [1] ++ unsafeCast ((cast acc * cast clamp) `mod` (cast lastPrime :: Stream Word64))

verifySpec :: Verbosity -> IO ()
verifySpec verb =
  do s <- reify multRingSpec
     r <- prove Z3 s
     when (verb P.>= Default) $
       print r
     verifyWithOptions defaultVerifierOptions{verbosity = verb}
                       mkDefaultCSettings ["reduced"] "multRingSpec" s

--verifySpec _ = interpret 10 engineMonitor
