-- XXX test until, release
-- XXX move the engineLTL example
-- XXX fix engine example in ppt slides

-- | Bounded Linear Temporal Logic (LTL) operators.  For a bound @n@, a property
-- @p@ holds if it holds on the next @n@ transitions (between periods).  If @n
-- == 0@, then the trace includes only the current period.  For example,
-- @
-- always 3 p
-- @
-- holds if @p@ holds at least once every four periods (3 transitions).  
-- 
-- Interface: see Examples/LTLExamples.hs You can embed a LTL specification
-- within a Copilot specification using the form:
-- @
--   var `ltl` (operator spec)
-- @
-- where 'var' is the variable you want to assign to the LTL specification
-- you're writing.
--
-- For some properties, stream dependencies may not allow their specification.
-- In particular, you cannot determine the "future" value of an external
-- variable.  In general, the ptLTL library is probaby more useful.

module Language.Copilot.Libs.LTL
  ( ltl, eventually, next, always, until, release 
  ) 
  where

import Prelude (Int, ($), String)
import qualified Prelude as P
import Data.List (foldl1)

import Language.Copilot.Core
import Language.Copilot.Language
import Language.Copilot.Libs.Indexes
import Language.Copilot.Libs.ErrorChks

ltl :: Var -> (Var -> Streams) -> Streams
ltl v f = f v

tmpName :: Var -> String -> Var
tmpName v name = v P.++ "_" P.++ name

-- | Property @s@ holds for the next @n@ periods.  We require @n >= 0@. If @n ==
-- 0@, then @s@ holds in the current period.  E.g., if @p = always 2 s@, then we
-- have the following relationship between the streams generated:
-- @
--      0 1 2 3 4 5 6 7
-- s => T T T F T T T T ...
-- p => T F F F T T ...
-- @
always :: Int -> Spec Bool -> Var -> Streams
always n s v = do
  v .= (nPosChk "always" n $ foldl1 (&&) [drop x (varB s') | x <- [0..n]])
  s' .= s
  where s' = tmpName v "always"

-- | Property @s@ holds at the next period.  For example:
-- @
--           0 1 2 3 4 5 6 7
-- s      => F F F T F F T F ...
-- next s => F F T F F T F ...
-- @
-- Note: s must have sufficient history to drop a value from it.
next :: Spec Bool -> Var -> Streams
next s v = do 
  v  .= drop 1 (varB s')
  s' .= s
  where s' = tmpName v "next"

-- | Property @s@ holds at some period in the next @n@ periods.  If @n == 0@,
-- then @s@ holds in the current period.  We require @n >= 0@.  E.g., if @p =
-- eventually 2 s@, then we have the following relationship between the streams
-- generated:
-- @
-- s => F F F T F F F T ...
-- p => F T T T F T T T ...
-- @
eventually :: Int -> Spec Bool -> Var -> Streams
eventually n s v = do
  v .= (nPosChk "eventually" n $ foldl1 (||) [drop x (varB s') | x <- [0..n]])
  s' .= s
  where s' = tmpName v "eventually"

-- | @until n s0 s1@ means that @eventually n s1@, and up until at least the
-- period before @s1@ holds, @s0@ continuously holds.
until :: Int -> Spec Bool -> Spec Bool -> Var -> Streams
until n s0 s1 v = do
  v'  .= (nPosChk "until" n $ 
            (   (whenS0 < 0) -- s0 didn't fail within n periods.
             || (soonest n (varB s1') <= whenS0))) -- s0 failed at some point before n.
  v'' `ltl` eventually n (varB s1') -- guarantees that soonest n s1 >= 0
  v   .= (varB v') && (varB v'')
  s0' .= s0
  s1' .= s1
  where 
    s0' = tmpName v "until_0"
    s1' = tmpName v "until_1"
    v''  = tmpName v "until_2"
    v' = tmpName v "until3"
    whenS0 = soonestFail n (varB s0')

-- | @release n s0 s1@ means that either @always n s1@, or @s1@ holds up to and
-- including the period at which @s0@ becomes true.
release :: Int -> Spec Bool -> Spec Bool -> Var -> Streams
release n s0 s1 v = do
  v' .= (nPosChk "release" n $ (   (whenS0 >= 0) -- s0 becomes true at some point
                                && (whenS0 < whenS1))) -- and when s0 becomes true,
                                                       -- is strictly sooner than
                                                       -- when s1 fails.
  v'' `ltl` always n (varB s1') -- s1 never fails
  v   .= (varB v') || (varB v'')
  s0' .= s0
  s1' .= s1
  where 
    s0' = tmpName v "release0"
    s1' = tmpName v "release1"
    v''  = tmpName v "release2"
    v' = tmpName v "release3"
    whenS1 = soonestFail n (varB s1')
    whenS0 = soonest n (varB s0')
