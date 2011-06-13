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
import Copilot.Compile.C99 (compile)
import Copilot.Library.Voting 

--------------------------------------------------------------------------------

--
-- Some utility functions:
--

--foo :: Stream Word32 -> Stream Word32
--foo x = if x == 0 then 2 else 3

--imply :: Bool -> Bool -> Bool
--imply p q = not p || q

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

sumExterns :: Stream Word64
sumExterns =
  let
    e1 = extern "e1"
    e2 = extern "e2"
  in
    e1 + e2

-- > interpret 10 [] vote 
-- results in out of memory
vote :: Spec
vote = do 
  trigger "maj" true
    [ arg maj ]
  trigger "aMaj" true 
    [ arg $ aMajority ls maj ]
  where
  ls  = [a, b, c, d, e, f, g, h, i, j, k, l, m]
  maj = majority ls
  a = [0] ++ a + 1 :: Stream Word32
  b = [0] ++ b + 1
  c = [0] ++ c + 1
  d = [0] ++ d + 1
  e = [1] ++ e + 1
  f = [1] ++ f + 1
  g = [1] ++ g + 1
  h = [1] ++ h + 1
  i = [1] ++ i + 1
  j = [1] ++ j + 1
  k = [1] ++ k + 1
  l = [1] ++ l + 1
  m = [1] ++ m + 1

--------------------------------------------------------------------------------

--
-- An example of a complete copilot specification.
--

-- A specification:
spec :: Spec 
spec =
  do
    -- A trigger with two arguments:
    trigger "f" booleans
      [ arg fib, arg sumExterns ]

    -- A trigger with a single argument:
    trigger "g" (flipflop booleans)
      [ arg (sumExterns + counter false + 25) ]

    -- A trigger with a single argument:
    trigger "h" (extern "e3" /= fib)
      [ arg (0 :: Stream Int8) ]

    -- A trigger with a single argument:
    trigger "i" true [arg $ odd nats]

-- Some infinite lists for simulating external variables:
e1, e2, e3 :: [Word64]
e1 = [0..]
e2 = 5 : 4 : e2
e3 = [1, 1] ++ zipWith (+) e3 (drop 1 e3)

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
    interpret 100 [input "e1" e1, input "e2" e2, input "e3" e3] spec

--------------------------------------------------------------------------------
