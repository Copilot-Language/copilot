{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds        #-}

-- | An example showing of using @copilot-verifier@ to verify a specification
-- involving arrays where individual elements are updated.
module Copilot.Verifier.Examples.ShouldPass.UpdateArray where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )

spec :: Spec
spec = do
  let pair :: Stream (Array 2 Word32)
      pair = extern "pair" Nothing

  -- Check equality, indexing into array and modifying the value. Note that
  -- this is trivial by equality.
  trigger "trig_1"
    (((pair !! 0 =$ (+1)) ! 0) == ((pair ! 0) + 1))
    [arg pair]

  -- Same as previous example, but get a different array index (so should be
  -- false).
  trigger "trig_2"
    (((pair !! 0 =$ (+1)) ! 1) == ((pair ! 0) + 1))
    [arg pair]

verifySpec :: Verbosity -> IO ()
verifySpec verb = reify spec >>= verifyWithOptions defaultVerifierOptions{verbosity = verb}
                                                   mkDefaultCSettings [] "updateArray"
