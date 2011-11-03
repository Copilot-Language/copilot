--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE RebindableSyntax #-}

module Main where

import Copilot.Library.Voting
import Copilot.Language
import Copilot.Language.Prelude

--------------------------------------------------------------------------------

vote :: Spec
vote = do 
  trigger "maj" true [ arg maj ]

  trigger "aMaj" true 
    [ arg $ aMajority xs maj ]

  where

  maj = majority xs
  xs = concat (replicate 1 ls)
  ls = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]

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
  n = [1] ++ n + 1
  o = [1] ++ o + 1
  p = [1] ++ p + 1
  q = [1] ++ q + 1
  r = [1] ++ r + 1
  s = [1] ++ s + 1
  t = [1] ++ t + 1
  u = [1] ++ u + 1
  v = [1] ++ v + 1
  w = [1] ++ w + 1
  x = [1] ++ x + 1
  y = [1] ++ y + 1
  z = [1] ++ z + 1

--------------------------------------------------------------------------------

main :: IO ()
main =
  do
    interpret 20 [] vote
--    prettyPrint vote
