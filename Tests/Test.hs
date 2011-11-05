--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | A very small test suite to check for basic functionality.

{-# LANGUAGE RebindableSyntax #-}

module Main where

import qualified Prelude as P
import Language.Copilot hiding (even, odd)
import Copilot.Compile.C99

import System.Directory (removeFile)
--------------------------------------------------------------------------------

--
-- Some utility functions:
--

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

bitWise :: Stream Word8
bitWise = ( let a = [ 1, 1, 0 ] ++ a in a )
          .^.
          ( let b = [ 0, 1, 1 ] ++ b in b )

sumExterns :: Stream Word64
sumExterns =
  let
    ex1 = extern "e1"
    ex2 = extern "e2"
  in
    ex1 + ex2

--------------------------------------------------------------------------------

--
-- An example of a complete copilot specification.


-- A specification:
spec :: Spec 
spec =
  do
    -- A trigger with four arguments:
    trigger "e" booleans
      [ arg fib, arg nats, arg sumExterns, arg bitWise ]

    -- A trigger with two arguments:
    trigger "f" booleans
      [ arg fib, arg sumExterns ]

    -- A trigger with a single argument:
    trigger "g" (flipflop booleans)
      [ arg (sumExterns + counter false + 25) ]

    -- A trigger with a single argument (should never fire):
    trigger "h" (extern "e3" /= fib)
      [ arg (0 :: Stream Int8) ]

    observer "i" (odd nats)

--- Some infinite lists for simulating external variables:
e1, e2, e3 :: [Word64]
e1 = [0..]
e2 = 5 : 4 : e2
e3 = [1, 1] P.++ zipWith (+) e3 (P.drop 1 e3)

main :: IO ()
main = do
  putStrLn "PrettyPrinter:"
  putStrLn ""
  prettyPrint spec
  putStrLn ""
  putStrLn ""
  putStrLn "Interpreter:"
  putStrLn ""
  interpret 10 [var "e1" e1, var "e2" e2, var "e3" e3] spec
  putStrLn ""
  putStrLn ""
  putStrLn "Atom compilation:"
  reify spec >>= compile defaultParams 
  cleanUp 
  putStrLn "*********************************"
  putStrLn " Ok, things seem to work.  Enjoy!"
  putStrLn "*********************************"

  -- Don't assume SBV is installed.
  -- putStrLn "Check equivalence:"
  -- putStrLn ""
  -- putStrLn ""
  -- reify spec >>= 
  --   C.genCBMC C.defaultParams {C.numIterations = 20}

--------------------------------------------------------------------------------

cleanUp :: IO ()
cleanUp = do
  removeFile "copilot.c"
  removeFile "copilot.h"
  return ()
