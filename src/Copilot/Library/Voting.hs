--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | An implementation of the Boyer-Moore Majority Vote Algorithm for Copilot.
--
-- For details of the Boyer-Moore Majority Vote Algorithm see the following
-- papers:
--
-- * Wim H. Hesselink,
-- \"The Boyer-Moore Majority Vote Algorithm\", 2005
--
-- * Robert S. Boyer and J Strother Moore,
-- \"MJRTY - A Fast Majority Vote Algorithm\", 1982

{-# LANGUAGE RebindableSyntax #-}

module Copilot.Library.Voting 
  ( majority, aMajority ) where

import Copilot.Language
import qualified Prelude as P

--------------------------------------------------------------------------------

majority :: (P.Eq a, Typed a) => [Stream a] -> Stream a
majority []     = badUsage "majority: empty list not allowed"
majority (x:xs) = majority' xs x 1

-- Alternate syntax of local bindings.
majority' :: (P.Eq a, Typed a)
   => [Stream a] -> Stream a -> Stream Word32 -> Stream a
majority' []     can _   = can
majority' (x:xs) can cnt =
  local (cnt == 0) inZero
  where 
  inZero zero    = local (if zero then x else can) inCan
    where       
    inCan can'   = local (if zero || x == can then cnt+1 else cnt-1) inCnt
      where 
      inCnt cnt' = majority' xs can' cnt'

--------------------------------------------------------------------------------

aMajority :: (P.Eq a, Typed a) => [Stream a] -> Stream a -> Stream Bool
aMajority [] _ = badUsage "aMajority: empty list not allowed"
aMajority xs can =
  let
    cnt = aMajority' 0 xs can
  in
    (cnt * 2) > fromIntegral (length xs)

aMajority' :: (P.Eq a, Typed a)
  => Stream Word32 -> [Stream a] -> Stream a -> Stream Word32
aMajority' cnt []     _   = cnt
aMajority' cnt (x:xs) can =
  local (if x == can then cnt+1 else cnt) $ \ cnt' ->
    aMajority' cnt' xs can
