--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE RebindableSyntax #-}

module Main where

import qualified Prelude as P
import Copilot.Language
import Copilot.Language.Prelude

--------------------------------------------------------------------------------

majority :: (Num a, Typed a) => [Stream a] -> Stream a
majority [] = error "Error in majority: list must be nonempty."
majority xs = majority' 0 xs 0 0

majority' :: (Num a, Typed a)
  => Int -> [Stream a] -> Stream a -> Stream Word32 -> Stream a

majority' _ []     candidate _   = candidate

majority' k (x:xs) candidate cnt =
  local (if cnt == 0 then x else candidate) $ \ candidate' ->
    local (if cnt == 0 || x == candidate then cnt+1 else cnt-1) $ \ cnt' ->
      majority' (k+1) xs candidate' cnt'

--------------------------------------------------------------------------------

vote :: Spec
vote = trigger "maj" true [ arg maj ]

  where

  maj = majority ls

  ls = [a, b, c, d, e, f, g, h, i, j, k]

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

--------------------------------------------------------------------------------

main :: IO ()
main =
  do
    interpret 20 [] vote
    prettyPrint vote
