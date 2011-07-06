--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RebindableSyntax #-}

module Main (main) where

import Copilot.Core.Random (randomSpec)
import Copilot.Core.Random.Weights (simpleWeights)
import Copilot.Compile.C99.Test (testCompilerAgainstInterpreter)
import Copilot.Language
import Copilot.Language.Prelude hiding (even)
import qualified Prelude as P
import Copilot.Language.Reify
import System.Random

--------------------------------------------------------------------------------

flipflop :: Stream Bool -> Stream Bool
flipflop x = y
  where
    y = [False] ++ if x then not y else y

nats :: Stream Word64
nats = [0] ++ nats + 1

even :: (P.Integral a, Typed a) => Stream a -> Stream Bool
even x = x `mod` 2 == 0

odd :: (P.Integral a, Typed a) => Stream a -> Stream Bool
odd = not . even

counter :: (Num a, Typed a) => Stream Bool -> Stream a
counter reset = y
  where
    zy = [0] ++ y
    y  = if reset then 0 else zy + 1

booleans :: Stream Bool
booleans = [True, True, False] ++ booleans

fib :: Stream Word64
fib = [1, 1] ++ fib + drop 1 fib

-- A specification:
spec :: Spec 
spec =
  do
    -- A trigger with two arguments:
    trigger "f" booleans
      [ arg fib, arg nats ]

    -- A trigger with a single argument:
    trigger "g" (flipflop booleans)
      [ arg (counter false + 25 :: Stream Int32) ]

--------------------------------------------------------------------------------

checkRandSpec :: IO Bool
checkRandSpec =
  do
    g <- newStdGen
    let spec = randomSpec simpleWeights g
    testCompilerAgainstInterpreter 20 spec

main :: IO ()
main =
  do
--    v <- checkRandSpec
    p <- reify spec
    v <- testCompilerAgainstInterpreter 3 p
    print v
