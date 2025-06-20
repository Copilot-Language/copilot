-------------------------------------------------------------------------------
-- Copyright © 2019 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Example showing an implementation of a resettable counter.

{-# LANGUAGE RebindableSyntax #-}

module Copilot.Verifier.Examples.ShouldPass.Counter where

import Control.Monad (when)
import qualified Prelude as P

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity(..), VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )
import Copilot.Theorem.What4 (prove, Solver(..))

-- A resettable counter
counter :: (Typed a, Integral a) => Stream Bool -> Stream Bool -> Stream a
counter inc reset = cnt
  where
    cnt = if reset then 0
          else if inc then z + 1
               else z
    z = [0] ++ cnt

-- Counter that resets when it reaches 256
bytecounter :: Stream Int32
bytecounter = counter true (resetcounter `mod` 256 == 0)

resetcounter :: Stream Word32
resetcounter = counter true false

bytecounter2 :: Stream Int32
bytecounter2 = counter true ([False] ++ bytecounter2 == 255)

spec :: Spec
spec =
  do _ <- prop "range" (forAll (bytecounter == unsafeCast (resetcounter `mod` 256)))
     _ <- prop "range2" (forAll (0 <= bytecounter2 && bytecounter2 <= 255))
     _ <- prop "same"  (forAll ((0 <= bytecounter2 && bytecounter2 <= 255) &&
                                (bytecounter == unsafeCast (resetcounter `mod` 256)) &&
                                (bytecounter == bytecounter2)))
     trigger "counter" true [arg $ bytecounter, arg $ bytecounter2]

verifySpec :: Verbosity -> IO ()
-- verifSpec _ = interpret 1280 spec
verifySpec verb =
  do s <- reify spec
     r <- prove Z3 s
     when (verb P.>= Default) $
       print r
     verifyWithOptions defaultVerifierOptions{verbosity = verb}
                       mkDefaultCSettings ["range", "range2"] "counter" s
