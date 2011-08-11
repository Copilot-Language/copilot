--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE RebindableSyntax #-}

module Main where

import qualified Prelude as P
import Copilot.Language
import Copilot.Language.Prelude hiding (even, odd)
import Copilot.Language.Reify (reify)

--------------------------------------------------------------------------------

--
-- Some utility functions:
--

implyStream :: Stream Bool -> Stream Bool -> Stream Bool
implyStream p q = not p || q

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
    e1 = extern "e1"
    e2 = extern "e2"
  in
    e1 + e2

--------------------------------------------------------------------------------

--
-- An example of a complete copilot specification.
--


-- A specification:
spec :: Spec 
spec =
  do

    -- A trigger with four arguments:
    trigger "f" true -- booleans
      [ arg fib, arg nats, arg sumExterns, arg bitWise ]

    -- A trigger with a single argument:
    trigger "g" (flipflop booleans)
      [ arg (sumExterns + counter false + 25) ]
--      [ arg (counter false + 25 :: Stream Int32) ]

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
main =
  do
    putStrLn "PrettyPrinter:"
    putStrLn ""
    prettyPrint spec
    putStrLn ""
    putStrLn ""
    putStrLn "Interpreter:"
    putStrLn ""
    interpret 10 [input "e1" e1, input "e2" e2, input "e3" e3] spec
    putStrLn ""
    putStrLn ""

--------------------------------------------------------------------------------
