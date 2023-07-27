-- |
-- Module: Voting
-- Description: Implementation of the Boyer-Moore Majority Vote Algorithm
-- Copyright: (c) 2011 National Institute of Aerospace / Galois, Inc.
--
-- This is an implementation of the Boyer-Moore Majority Vote Algorithm for
-- Copilot, which solves the majority vote problem in linear time and constant
-- memory in two passes. 'majority' implements the first pass, and 'aMajority'
-- the second pass. For details of the Boyer-Moore Majority Vote Algorithm see
-- the following papers:
--
-- * <http://www.cs.rug.nl/~wim/pub/whh348.pdf Wim H. Hesselink, \"The Boyer-Moore Majority Vote Algorithm\", 2005>
--
-- * <ftp://net9.cs.utexas.edu/pub/techreports/tr81-32.pdf Robert S. Boyer and J Strother Moore, \"MJRTY - A Fast Majority Vote Algorithm\", 1981>
--
-- In addition, <https://copilot-language.github.io/downloads/copilot_tutorial.pdf An Introduction to Copilot> and
-- <https://github.com/Copilot-Language/copilot-discussion copilot-discussion> explain
-- a form of this code in section 4.
--
-- For instance, with four streams passed to 'majority', and the candidate stream
-- then passed to 'aMajority':
--
-- @
-- vote1:       vote2:       vote3:       vote4:       majority:    aMajority:
-- 0            0            0            0            0            true
-- 1            0            0            0            0            true
-- 1            1            0            0            1            false
-- 1            1            1            0            1            true
-- 1            1            1            1            1            true
-- @
--
-- For other examples, see @examples/Voting.hs@ in the
-- <https://github.com/Copilot-Language/copilot/blob/master/copilot/examples/ Copilot repository>.

{-# LANGUAGE RebindableSyntax #-}

module Copilot.Library.Voting
  ( majority, aMajority ) where

import Copilot.Language
import qualified Prelude as P

-- | Majority vote first pass: choosing a candidate.
majority :: (P.Eq a, Typed a) =>
            [Stream a] -- ^ Vote streams
            -> Stream a -- ^ Candidate stream
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
    inCan can' =
        -- We include a special case for when `xs` is empty that immediately
        -- returns `can'`. We could omit this special case without changing the
        -- final result, but this has the downside that `local` would bind a
        -- local variable that would go unused in `inCnt`. (Note that `inCnt`
        -- recursively invokes `majority'`, which doesn't use its last argument
        -- if the list of vote streams is empty.) These unused local variables
        -- would result in C code that triggers compiler warnings.
        case xs of
          [] -> can'
          _  -> local (if zero || x == can then cnt+1 else cnt-1) inCnt
      where
      inCnt cnt' = majority' xs can' cnt'

-- | Majority vote second pass: checking that a candidate indeed has more than
-- half the votes.
aMajority :: (P.Eq a, Typed a) =>
             [Stream a] -- ^ Vote streams
             -> Stream a -- ^ Candidate stream
             -> Stream Bool -- ^ True if candidate holds majority
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
