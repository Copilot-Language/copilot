--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Copilot.Library.Voting
  ( majority
  , aMajority
  ) where

import Prelude ()
import Data.List (foldl')
import Copilot.Language.Prelude
import Copilot.Language

-- | Boyer-Moore linear majority algorithm.  Warning!  Returns an arbitary
-- element if no majority is returned.  See 'aMajority' below.
majority :: (Num a, Typed a) => [Stream a] -> Stream a
majority [] = error "Error in majority: list must be nonempty."
majority xs = majority' xs 0 0

majority' :: (Num a, Typed a) 
          => [Stream a] -> Stream a -> Stream Word32 -> Stream a
majority' [] candidate _ = candidate
majority' (x:xs) candidate cnt = 
  majority' xs (if eqZero then x else candidate)
               (if eqZero || x == candidate
                  then cnt + 1 
                  else cnt - 1) 
  where 
  eqZero = cnt == 0 :: Stream Bool

-- | Is the candidate majority value the actual majority value?
aMajority :: (Num a, Typed a) => [Stream a] -> Stream a -> Stream Bool
aMajority [] _ = 
  error "Error in aMajority: list must be nonempty."
aMajority ls candidate = 
  let m = map (\x -> if (x == candidate) :: Stream Bool 
                       then 1 :: Stream Word32 
                       else 0
              ) ls in
  ((foldl' (+) 0 m) * 2 :: Stream Word32) > (fromIntegral $ length ls)
