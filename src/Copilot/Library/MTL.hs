-- Metric Temporal Logic (MTL) operators over a discrete time
-- domain consisting of sampled time values

module Copilot.Library.MTL
  ( eventually, eventuallyPrev, always, alwaysBeen,
    until, release, since, Copilot.Library.MTL.trigger, matchingUntil,
    matchingRelease, matchingSince, matchingTrigger ) where

import Copilot.Language
import qualified Prelude as P
import Copilot.Library.Utils

-- It is necessary to provide a number of time units dist
-- to each function, where the distance between the times
-- of any two adjacent clock samples is no less than dist

-- Eventually: True at time t iff s is true at some time t',
-- where (t + l) <= t' <= (t + u)
eventually :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool
eventually l u clk dist s = res clk s ((u `P.div` dist) + 1)
  where
  mins = (clk + (constant l))
  maxes = (clk + (constant u))
  res _ _ 0 = false
  res c s k =
    mux (mins <= c && c <= maxes) (s || (nextRes c s k)) (nextRes c s k)
  nextRes c s k = res (drop 1 c) (drop 1 s) (k - 1)

-- EventuallyPrev: True at time t iff s is true at some time t',
-- where (t - u) <= t' <= (t - l)
eventuallyPrev :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool
eventuallyPrev l u clk dist s = res clk s ((u `P.div` dist) + 1)
  where
  mins = (clk - (constant u))
  maxes = (clk - (constant l))
  res _ _ 0 = false
  res c s k =
    mux (mins <= c && c <= maxes) (s || (nextRes c s k)) (nextRes c s k)
  nextRes c s k = res ([0] ++ c) ([False] ++ s) (k - 1)

-- Always: True at time t iff s is true at all times t'
-- where (t + l) <= t' <= (t + u)
always :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool
always l u clk dist s = res clk s ((u `P.div` dist) + 1) 
  where 
  mins = (clk + (constant l)) 
  maxes = (clk + (constant u)) 
  res _ _ 0 = true 
  res c s k = 
    mux (mins <= c && c <= maxes) (s && (nextRes c s k)) (nextRes c s k)
  nextRes c s k = res (drop 1 c) (drop 1 s) (k - 1)

-- AlwaysBeen: True at time t iff s is true at all times t'
-- where (t - u) <= t' <= (t - l)
alwaysBeen :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool
alwaysBeen l u clk dist s = res clk s ((u `P.div` dist) + 1)
  where
  mins = (clk - (constant u))
  maxes = (clk - (constant l))
  res _ _ 0 = true
  res c s k =
    mux (mins <= c && c <= maxes) (s && (nextRes c s k)) (nextRes c s k)
  nextRes c s k = res ([0] ++ c) ([False] ++ s) (k - 1)

-- Until: True at time t iff there exists a d with l <= d <= u
-- such that s1 is true at time (t + d),
-- and for all times t' with t <= t' < t + d, s0 is true
until :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool -> Stream Bool
until l u clk dist s0 s1 = res clk s0 s1 ((u `P.div` dist) + 1)
  where
  mins = (clk + (constant l))
  maxes = (clk + (constant u))
  res _ _ _ 0 = false
  res c s s' k =
    mux (mins <= c && c <= maxes)
      (s' || (s && (nextRes c s s' k)))
      ((c < mins) && s && (nextRes c s s' k))
  nextRes c s s' k = res (drop 1 c) (drop 1 s) (drop 1 s') (k - 1)

-- Since: True at time t iff there exists a d with l <= d <= u
-- such that s1 is true at time (t - d),
-- and for all times t' with t - d < t' <= t, s0 is true
since :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool -> Stream Bool
since l u clk dist s0 s1 = res clk s0 s1 ((u `P.div` dist) + 1)
  where
  mins = (clk - (constant u))
  maxes = (clk - (constant l))
  res _ _ _ 0 = false 
  res c s s' k =
    mux (mins <= c && c <= maxes)
      (s' || (s && (nextRes c s s' k)))
      ((c > maxes) && s && (nextRes c s s' k))
  nextRes c s s' k = res ([0] ++ c) ([False] ++ s) ([False] ++ s') (k - 1)

-- Release: true at time t iff for all d with l <= d <= u where there
-- is a sample at time (t + d), s1 is true at time (t + d),
-- or s0 is true at some time t' with t <= t' < t + d
release :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool -> Stream Bool
release l u clk dist s0 s1 = res clk s1 iter
  where
  mins = (clk + (constant l))
  maxes = (clk + (constant u))
  iter = (u `P.div` dist) + 1
  res _ _ 0 = true
  res c s k = 
    mux (mins > c || c > maxes || s)
      (nextRes c s k)
      (res' clk s0 iter c)
  nextRes c s k = res (drop 1 c) (drop 1 s) (k - 1)
  res' _ _ 0 _ = false
  res' c s k upl = ((c < upl) && s) || (nextRes' c s k upl)
  nextRes' c s k upl = res' (drop 1 c) (drop 1 s) (k - 1) upl

-- Trigger: True at time t iff for all d with l <= d <= u where there
-- is a sample at time (t - d), s1 is true at time (t - d),
-- or s0 is true at some time t' with t - d < t' <= t 
trigger :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool -> Stream Bool
trigger l u clk dist s0 s1 = res clk s1 iter
  where
  mins = (clk - (constant u))
  maxes = (clk - (constant l))
  iter = (u `P.div` dist) + 1
  res _ _ 0 = true
  res c s k =
    mux (mins > c || c > maxes || s)
      (nextRes c s k)
      (res' clk s0 iter c)
  nextRes c s k = res ([0] ++ c) ([False] ++ s) (k - 1)
  res' _ _ 0 _ = false
  res' c s k lowl = ((c > lowl) && s) || (nextRes' c s k lowl)
  nextRes' c s k lowl = res' ([0] ++ c) ([False] ++ s) (k - 1) lowl

-- Matching Variants

-- Matching Until: Same semantics as Until, except with both s1 and s0
-- needing to hold at time (t + d) instead of just s1
matchingUntil :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool -> Stream Bool
matchingUntil l u clk dist s0 s1 = res clk s0 s1 ((u `P.div` dist) + 1)
  where
  mins = (clk + (constant l))
  maxes = (clk + (constant u))
  res _ _ _ 0 = false
  res c s s' k =
    mux (mins <= c && c <= maxes)
      (s && (s' || (nextRes c s s' k)))
      ((c < mins) && s && (nextRes c s s' k))
  nextRes c s s' k = res (drop 1 c) (drop 1 s) (drop 1 s') (k - 1)

-- Matching Since: Same semantics as Since, except with both s1 and s0
-- needing to hold at time (t - d) instead of just s1
matchingSince :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool -> Stream Bool
matchingSince l u clk dist s0 s1 = res clk s0 s1 ((u `P.div` dist) + 1)
  where
  mins = (clk - (constant u))
  maxes = (clk - (constant l))
  res _ _ _ 0 = false 
  res c s s' k =
    mux (mins <= c && c <= maxes)
      (s && (s' || (nextRes c s s' k)))
      ((c > maxes) && s && (nextRes c s s' k))
  nextRes c s s' k = res ([0] ++ c) ([False] ++ s) ([False] ++ s') (k - 1)

-- Matching Release: Same semantics as Release, except with
-- s1 or s0 needing to hold at time (t + d) instead of just s1
matchingRelease :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool -> Stream Bool
matchingRelease l u clk dist s0 s1 = res clk s0 s1 iter
  where
  mins = (clk + (constant l))
  maxes = (clk + (constant u))
  iter = (u `P.div` dist) + 1
  res _ _ _ 0 = true
  res c s s' k = 
    mux (mins > c || c > maxes || (s' && s))
      (nextRes c s s' k)
      (res' clk s0 iter c)
  nextRes c s s' k = res (drop 1 c) (drop 1 s) (drop 1 s') (k - 1)
  res' _ _ 0 _ = false
  res' c s k upl = ((c < upl) && s) || (nextRes' c s k upl)
  nextRes' c s k upl = res' (drop 1 c) (drop 1 s) (k - 1) upl

-- Matching Trigger: Same semantics as Trigger, except with
-- s1 or s0 needing to hold at time (t - d) instead of just s1
matchingTrigger :: ( Typed a, Integral a ) =>
  a -> a -> Stream a -> a -> Stream Bool -> Stream Bool -> Stream Bool
matchingTrigger l u clk dist s0 s1 = res clk s0 s1 iter
  where
  mins = (clk - (constant u))
  maxes = (clk - (constant l))
  iter = (u `P.div` dist) + 1
  res _ _ _ 0 = true
  res c s s' k =
    mux (mins > c || c > maxes || (s' && s))
      (nextRes c s s' k)
      (res' clk s0 iter c)
  nextRes c s s' k = res ([0] ++ c) ([False] ++ s) ([False] ++ s') (k - 1)
  res' _ _ 0 _ = false
  res' c s k lowl = ((c > lowl) && s) || (nextRes' c s k lowl)
  nextRes' c s k lowl = res' ([0] ++ c) ([False] ++ s) (k - 1) lowl
