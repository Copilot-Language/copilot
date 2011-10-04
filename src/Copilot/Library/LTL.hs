-- | Bounded Linear Temporal Logic (LTL) operators.  For a bound @n@, a property
-- @p@ holds if it holds on the next @n@ transitions (between periods).  If
-- @n == 0@, then the trace includes only the current period.  For example,
-- @
-- eventually 3 p
-- @
-- holds if @p@ holds at least once every four periods (3 transitions).
--
-- Interface: see Examples/LTLExamples.hs You can embed an LTL specification
-- within a Copilot specification using the form:
-- @
--   operator spec
-- @
--
-- For some properties, stream dependencies may not allow their specification.
-- In particular, you cannot determine the "future" value of an external
-- variable.  In general, the ptLTL library is probaby more useful.

{-# LANGUAGE NoImplicitPrelude #-}

module Copilot.Library.LTL
  ( next, eventually, always, until, release ) where

import Copilot.Language
import Copilot.Language.Prelude
import Copilot.Library.Utils


-- | Property @s@ holds at the next period.  For example:
-- @
--           0 1 2 3 4 5 6 7
-- s      => F F F T F F T F ...
-- next s => F F T F F T F ...
-- @
-- Note: s must have sufficient history to drop a value from it.
next :: Stream Bool -> Stream Bool
next = drop ( 1 :: Int )


-- | Property @s@ holds for the next @n@ periods.  We require @n >= 0@. If @n ==
-- 0@, then @s@ holds in the current period.  E.g., if @p = always 2 s@, then we
-- have the following relationship between the streams generated:
-- @
--      0 1 2 3 4 5 6 7
-- s => T T T F T T T T ...
-- p => T F F F T T ...
-- @
always :: ( Integral a ) => a -> Stream Bool -> Stream Bool
always n = nfoldl1 ( fromIntegral n + 1 ) (&&)


-- | Property @s@ holds at some period in the next @n@ periods.  If @n == 0@,
-- then @s@ holds in the current period.  We require @n >= 0@.  E.g., if @p =
-- eventually 2 s@, then we have the following relationship between the streams
-- generated:
-- @
-- s => F F F T F F F T ...
-- p => F T T T F T T T ...
-- @
eventually :: ( Integral a ) => a -> Stream Bool -> Stream Bool
eventually n = nfoldl1 ( fromIntegral n + 1 ) (||)


-- | @until n s0 s1@ means that @eventually n s1@, and up until at least the
-- period before @s1@ holds, @s0@ continuously holds.
until :: ( Integral a ) => a -> Stream Bool -> Stream Bool -> Stream Bool
until n s0 s1 = foldl1 (||) v0
    where n' = fromIntegral n
          v0 = [ always ( i :: Int ) s0 && drop ( i + 1 ) s1
               | i <- [ 0 .. n' - 1 ]
               ]


-- | @release n s0 s1@ means that either @always n s1@, or @s1@ holds up to and
-- including the period at which @s0@ becomes true.
release :: ( Integral a ) => a -> Stream Bool -> Stream Bool -> Stream Bool
release n s0 s1 = always n s1 || foldl1 (||) v0
    where n' = fromIntegral n
          v0 = [ always ( i :: Int ) s1 && drop i s0
               | i <- [ 0 .. n' - 1 ]
               ]
