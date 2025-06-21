--------------------------------------------------------------------------------
-- Copyright © 2019 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Example showing usage of clocks to generate periodically recurring truth
-- values.

module Copilot.Verifier.Examples.ShouldPass.Clock where

import Control.Monad (when)
import qualified Prelude as P

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity(..), VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )
import Copilot.Theorem.What4 (prove, Solver(..))

-- | We need to force a type for the argument of `period`.
p :: Word8
p = 5

-- | Both have the same period, but a different phase.
clkStream :: Stream Bool
clkStream  = clk (period p) (phase 0)

clkStream' :: Stream Bool
clkStream' = clk (period p) (phase 2)

spec :: Spec
spec = do
  observer "clk"  clkStream
  observer "clk'" clkStream'
  _ <- prop "clksPhase" (forAll (clkStream == drop 2 clkStream'))
  _ <- prop "clksDistinct" (forAll (not (clkStream && clkStream')))
  trigger "clksHigh" (clkStream && clkStream') []


verifySpec :: Verbosity -> IO ()
verifySpec verb =
  do s <- reify spec
     r <- prove Z3 s
     when (verb P.>= Default) $
       print r
     verifyWithOptions defaultVerifierOptions{verbosity = verb}
                       mkDefaultCSettings [] "clock" s
