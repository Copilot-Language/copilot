{-# LANGUAGE DataKinds #-}
module Copilot.Verifier.Examples.ShouldPass.ArrayGen where

import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )
import Language.Copilot
import qualified Prelude hiding ((++), (>))

spec :: Spec
spec = trigger "f" (stream ! 0 > 0) [arg stream]
  where
    stream :: Stream (Array 2 Int16)
    stream = [array [3,4]] ++ rest

    rest :: Stream (Array 2 Int16)
    rest = constant $ array [5,6]

verifySpec :: Verbosity -> IO ()
verifySpec verb = reify spec >>= verifyWithOptions defaultVerifierOptions{verbosity = verb}
                                                   mkDefaultCSettings [] "arrayGen"
