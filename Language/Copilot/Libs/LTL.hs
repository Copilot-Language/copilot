-- | Bounded Linear Temporal Logic (LTL) operators.

module Language.Copilot.Libs.LTL(always, next, eventually, until, release) where

import Prelude (Int, ($))
import qualified Prelude as P 
import Data.List(foldl1)

import Language.Copilot.Core
import Language.Copilot.Language
import Language.Copilot.Libs.Indexes
import Language.Copilot.Libs.ErrorChks

-- | Property @s@ holds for the next @n@ periods.  If @n == 0@, then @s@ holds
-- in the current period.  We require @n >= 0@.
-- E.g., if @p = always 2 s@, then we have the following relationship between the streams generated:
-- @
-- s => T T T T F F F F ...
-- p => T T F F F F F F ...
-- @
always :: Int -> Spec Bool -> Spec Bool
always n s = nPosChk "always" n $ foldl1 (&&) [drop x s | x <- [0..n]]

-- | Property @s@ holds at the next period.
next :: Spec Bool -> Spec Bool
next s = drop 1 s

-- | Property @s@ holds at some period in the next @n@ periods.  If @n == 0@,
-- then @s@ holds in the current period.  We require @n >= 0@.
-- E.g., if @p = eventually 2 s@, then we have the following relationship between the streams generated:
-- @
-- s => F F F F T F T F ...
-- p => F F T T T T T T ...
-- @
eventually :: Int -> Spec Bool -> Spec Bool
eventually n s = nPosChk "eventually" n $ foldl1 (||) [drop x s | x <- [0..n]] 

-- | @until n s0 s1@ means that @eventually n s1@, and up until at least the
-- period before @s1@ holds, @s0@ continuously holds.
until :: Int -> Spec Bool -> Spec Bool -> Spec Bool
until n s0 s1 = 
  nPosChk "until" n $    (   (whenS0 < const 0) -- s0 didn't fail within n periods.
                          || (soonest n s1 <= whenS0)) -- s0 failed at some point before n.
                      && eventually n s1 -- guarantees that soonest n s1 >= 0
  where whenS0 = soonestFail n s0

-- | @release n s0 s1@ means that either @always n s1@, or @s1@ holds up to and
-- including the period at which @s0@ becomes true.
release :: Int -> Spec Bool -> Spec Bool -> Spec Bool
release n s0 s1 = 
  nPosChk "release" n $    (whenS1 < const 0) -- s1 never fails
                        || (   (whenS0 >= const 0) -- s0 becomes true at some point
                            && (whenS0 < whenS1)) -- and when s0 becomes true,
                                                  -- is strictly sooner than
                                                  -- whne s1 fails.
  where whenS1 = soonestFail n s1
        whenS0 = soonest n s0
