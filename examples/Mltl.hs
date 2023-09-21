{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

module Mltl where

import Language.Copilot hiding  (alwaysBeen, eventuallyPrev, since)

import qualified Prelude (take, repeat, (<), (>), (++), (==))

-- FTLTL impl
-- 0 corresponds to False
-- 1 corresponds to Presumably False
-- 2 corresponds to Presumably True
-- 3 corresponds to True
fc :: Int8 -- false
fc = 0

f :: Stream Int8
f = constI8 fc

pfc :: Int8 -- presumably false
pfc = 1

pf :: Stream Int8
pf = constI8 pfc

ptc :: Int8 -- presumably false
ptc = 2

pt :: Stream Int8
pt = constI8 ptc

tc :: Int8 -- true
tc = 3

t :: Stream Int8
t = constI8 tc

-- UB behavior, should never occur
errC :: Int8
errC = -1

err :: Stream Int8
err = constI8 errC

and' :: Stream Int8 -> Stream Int8 -> Stream Int8
and' s1 s2 = if (s1 < s2) then s1 else s2 -- refactor out

or' :: Stream Int8 -> Stream Int8 -> Stream Int8
or' s1 s2 = if (s1 < s2) then s2 else s1 -- refactor out

not' :: Stream Int8 -> Stream Int8
not' s1 = if (s1 == f) then t else 
  (if (s1 == pf) then pt else 
  (if (s1 == pt) then pf else 
  (if (s1 == t) then (f) else err)))

implies' :: Stream Int8 -> Stream Int8 -> Stream Int8
implies' s1 s2 = not' (and' s1 (not' s2))


-- mask everything but the first
maskFirst :: Stream Int8 -> Stream Int8
maskFirst s = and' mask s
  where
    mask = ([tc] ++ f)
    
maskSecond :: Stream Int8 -> Stream Int8
maskSecond s = and' mask s
  where
    mask = ([fc, tc] ++ f)

takeHighest :: Stream Int8 -> Stream Int8
takeHighest s = go
  where
    go = or' s ([fc] ++ go)
    
takeHighestPf :: Stream Int8 -> Stream Int8
takeHighestPf s = go
  where
    go = or' s ([pfc] ++ go)
    
takeLowest :: Stream Int8 -> Stream Int8
takeLowest s = go
  where
    go = and' s ([tc] ++ go)

-- formal spec
-- sound but not precise (since we cannot "peek" ahead even if taut)
eNext :: Stream Int8 -> Stream Int8
eNext s = or' ([pfc] ++ f)
               (takeHighest (maskSecond s))

uNext :: Stream Int8 -> Stream Int8
uNext s = or' ([ptc] ++ f)
              (takeHighest (maskSecond s))


-- can be falsified, but vaculously presumably true
globally' :: Stream Int8 -> Stream Int8
-- globally' s = and' pt (takeLowest s)
globally' s = go s where
  go s = and' (go ([ptc] ++ s)) (takeLowest s)
  -- go s = and' ([ptc] ++ (go s)) (takeLowest s)

-- can never be falsified, lowest is pfc, vaculously presumably false
finally :: Stream Int8 -> Stream Int8
finally s = or' pf (takeHighest s)

-- does not need stop
weakUntil' :: Stream Int8 -> Stream Int8 -> Stream Int8
weakUntil' s1 s2 = globally' 
  (or' (globally' s1) ( (takeHighest s2)))
  -- (or' (globally' s1) (eNext s2))
  -- maybe swap takeHighest with finally

-- needs to stop
strongUntil' :: Stream Int8 -> Stream Int8 -> Stream Int8
strongUntil' s1 s2 = takeHighest (and' (weakUntil' s1 s2) s2)

-- will mask at interval [m, n]
maskNthTrueRange :: Int -> Int -> Stream Int8 -> Stream Int8
maskNthTrueRange m n s = or' mask s
    where
      mask = ((Prelude.take m (Prelude.repeat tc)) Prelude.++ (Prelude.take (n - m) (Prelude.repeat fc))) ++ t

maskNthFalseRange :: Int -> Int -> Stream Int8 -> Stream Int8
maskNthFalseRange m n s = and' mask s
    where
      mask = ((Prelude.take m (Prelude.repeat fc)) Prelude.++ (Prelude.take (n - m) (Prelude.repeat tc))) ++ f

globallyWithin :: Int -> Int -> Stream Int8 -> Stream Int8
globallyWithin m n s = and' streamMask defaultPresumablyTrueMask
  where
    defaultPresumablyTrueMask = maskNthTrueRange 0 n pt
    streamMask = takeLowest (maskNthTrueRange m n s)

-- todo: double check below, dual to above
finallyWithin :: Int -> Int -> Stream Int8 -> Stream Int8
finallyWithin m n s = or' streamMask defaultPresumablyFalseMask
  where
    defaultPresumablyFalseMask = maskNthFalseRange 0 n pf
    streamMask = takeHighest (maskNthFalseRange m n s)

-- todo: double check the below
weakUntilWithin :: Int -> Int -> Stream Int8 -> Stream Int8 -> Stream Int8
weakUntilWithin m n s1 s2 = or' (globallyWithin m n s1) (finallyWithin m n s2)

strongUntilWithin :: Int -> Int -> Stream Int8 -> Stream Int8 -> Stream Int8
strongUntilWithin m n s1 s2 = finallyWithin m n (and' (weakUntilWithin m n s1 s2) s2)

-- PTLTL port from Copilot lang (need to double check)
previous :: Stream Int8 -> Stream Int8
previous s = [ fc ] ++ s

alwaysBeen :: Stream Int8 -> Stream Int8
alwaysBeen s = and' s tmp
    where
      tmp = and' ([ tc ] ++ s) tmp

-- ever used to be named eventuallyPre, ported from PTLTL
ever :: Stream Int8 -> Stream Int8
ever s = or' s tmp
  where
    tmp = or' ([ fc ] ++ s) tmp

-- everything below is mine
-- todo: check if equiv
-- used to be named historically
alwaysBeenMine :: Stream Int8 -> Stream Int8
alwaysBeenMine s = go
  where 
    go = and' s ([ tc ] ++ go)

everMine :: Stream Int8 -> Stream Int8
everMine s = go
  where
    go = or' s ([ fc ] ++ go)

sinceMine :: Stream Int8 -> Stream Int8 -> Stream Int8
sinceMine s1 s2 = everMine (implies' s2 (alwaysBeenMine s1))


specFTLTL = do
  observer "s1" input1
  observer "s2" input2
  -- observer "weakUntil" (weakUntilWithin 1 5 input1 input2)
  -- observer "strongUntilWithin" (strongUntilWithin 1 5 input1 input2)
  -- observer "globallyWithin s1" (globallyWithin 3 4 input1)
  -- observer "finallyWithin s1" (finallyWithin 2 4 input1)
  -- observer "input2" input2
  -- observer "not1" (not' input1)
  -- observer "not2" (not' input2)
  -- observer "identity1" (not' (strongUntil' input1 input2))
  -- observer "identity2'" (weakUntil' (not' input1) (not' input2))
  -- observer "globally input1" (globally' input1)
  observer "implies globally" (implies' (globally' input1) input2)
  observer "globally implies globally" (globally' (implies' (globally' input1) input2))
  -- observer "finally input2" (finally input2)
  -- observer "weakUntil'" (weakUntil' input1 input2)
  -- observer "strongUntil'" (strongUntil' input1 input2)
  -- observer "strongUntilTmp" (strongUntilTmp input1 input2)
  -- observer "maskFirst input1" (maskFirst input1)
  -- observer "takeHighest input1" (takeHighest input1)
  -- observer "input2" input2
  -- observer "universalNext' input2" (universalNext' input2)
  where
    input1 = [fc, tc, tc] ++ t
    -- input1 = f
    -- input2 = f
    -- input2 = [fc, pfc, tc] ++ f
    input2 = [tc, tc, fc] ++ f
    -- input1 = pf
    -- input1 = [3, 1, 1] ++ f
    -- input1 = [0, 0, 3] ++ f
    -- input1 = [0] ++ t
    -- input1 = [3] ++ f
    -- input1 = t

-- Interpret the spec for 10 ticks
main = interpret 10 specFTLTL
