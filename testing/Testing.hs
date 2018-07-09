{-
 - Basic file containing a main function, used for testing the compilation
 -}

module Main where

import Language.Copilot

import Copilot.Compile.C
import qualified Prelude as P


arr :: Int8 -> Array Int Int8
arr start = array l (zip (indices l) (P.take l [start..])) where
  l = 3

{-a :: Array Int Int8
a = array 5 (zip (indices 5) [10..15])
b = array 5 (zip (indices 5) [20..25])
c = array 5 (zip (indices 5) [30..35])
d = array 5 (zip (indices 5) [40..45])-}

a = arr 0
b = arr 10
c = arr 20
d = arr 30
e = arr 40
f = arr 50

spec :: Spec
spec = do
  {-let s0 :: Stream Int32
      s0 = [1,2] ++ s1
      s1 = [8,9] ++ s0

      ar :: Stream (Array Int Int8)
      ar = [a, b, c] ++ ar'
      ar' = [d, e, f] ++ ar-}

  let arr = [a, b] ++ arr'
      arr' = [c, d] ++ arr

      n :: Stream Int8
      n = [1] ++ (n + 1)

  trigger "farray" true [arg n, arg arr, arg arr']


main :: IO ()
main = do reify spec >>= compile
