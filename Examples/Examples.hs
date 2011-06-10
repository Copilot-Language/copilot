{-# LANGUAGE RebindableSyntax #-}

module Main where

import Prelude ()
import Copilot.Language
import Copilot.Language.Prelude hiding (even)
import Copilot.Language.Reify (reify)
import Copilot.Compile.C99 (compile)

--
-- Some utility functions:
--

imply :: Bool -> Bool -> Bool
imply p q = not p || q

flipflop :: Stream Bool -> Stream Bool
flipflop x = y
  where
    y = [False] ++ if x then not y else y

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

--
-- An example of a specification:
--

spec :: Copilot
spec = do
  -- first trigger:
  trigger "f" booleans
    [ arg fib
    , arg sumExterns ]

  -- second trigger:
  trigger "g" (flipflop booleans)
    [ arg (sumExterns + counter false + 25) ]

  -- this trigger shouldn't fire:
  trigger "h" (extern "e3" /= fib)
    [ arg (0 :: Stream Int8) ]

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
