{-# LANGUAGE FlexibleContexts #-}

module Language.Copilot.Examples.Examples where

import Data.Word
import Prelude (($))
import qualified Prelude as P
import qualified Prelude as Prelude

-- for specifying options
import Data.Map (fromList) 
import Data.Maybe (Maybe (..))
import System.Random
import Data.Int

import Language.Copilot.Core
import Language.Copilot.Language
import Language.Copilot.Interface
import Language.Copilot.Variables
import Language.Copilot.PrettyPrinter

import Language.Copilot.Libs.LTL

fib :: Streams
fib = do
  "fib" .= [0,1] ++ var "fib" + (drop 1 $ varW64 "fib")
  "t"   .= even (var "fib")
    where even :: Spec Word64 -> Spec Bool
          even w' = w' `mod` const 2 == const 0

t1 :: Streams
t1 = do
     x .= [0, 1, 2] ++ var x - (drop 1 $ varI32 x)
     y .= [True, False] ++ var y ^ var z
     z .= varI32 "x" <= drop 1 (var x)

t2 :: Streams
t2 = do
     a .= [True] ++ not (var a) 
     b .= mux (var a) 2 (int8 3) 

-- t3 :: use an external variable called ext, typed Word32
t3 :: Streams
t3 = do
     a .= [0,1] ++ (var a) + extW32 "ext" 8 + extW32 "ext" 8 + extW32 "ext" 1
     b .= [True, False] ++ 2 + var a < 5 + extW32 "ext" 1

t4 :: Streams
t4 = do
     a .= [True,False] ++ not (var a)
     b .= drop 1 (varB a)

t5 :: Streams
t5 = do
    x .= drop 3 (varB y)
    y .= [True, True] ++ not (var z)
    z .= [False, False] ++ not (var z)
    w .= varB x || var y

yy :: Streams
yy = do a .= constW64 4  

zz :: Streams
zz = do --a .= [0..4] ++ drop 4 (varW32 a) + 1
  a .= (varW32 a) + 1
  b .= drop 3 (varW32 a)

xx :: Streams
xx = do 
     a .= extW32 "ext" 5 
     b .= [3] ++ (varW32 a)
     c .= [0, 1, 3, 4] ++ drop 1 (varW32 b)

-- If the temperature rises more than 2.3 degrees within 0.2 seconds, then the
-- engine is immediately shut off.  From the paper.
engine :: Streams
engine = do
   "temps" .= [0, 0, 0] ++ extF "temp" 1
   "overTempRise" .= drop 2 (varF "temps") > const 2.3 + var "temps"
   "trigger" .= (var "overTempRise") ==> (extB "shutoff" 2) 

-- To compile: > let (streams, ss) = dist in interface $ compileOpts streams ss "dist"
-- s at phase 2 on port 1.  Not stable.
dist :: DistributedStreams
dist = 
  ( a .= [0,1] ++ (var a) + constW8 1
  ,     sendW8 a (2, 1)
    ..| emptySM
  )

-- greatest common divisor.
gcd :: Word16 -> Word16 -> Streams
gcd n0 n1 = do 
  a .= alg n0 a b
  b .= alg n1 b a
  "ans" .= varW16 a == var b
    where alg x0 x1 x2 = [x0] ++ mux (varW16 x1 > var x2) (var x1 - var x2) (var x1)

-- greatest common divisor of two external vars.  Compare to
-- Language.Atom.Example Try 
--
-- interpret gcd' 40 $ setE (emptySM {w16Map = 
--      fromList [("n", [9,9..]), ("m", [7,7..])]}) baseOpts 
--
-- Note we have to start streams a and b with a dummy value 0 before they can
-- sample the external variables.
gcd' :: Streams
gcd' = do 
  a .= alg "n" (sub a b)
  b .= alg "m" (sub b a)
  "ans" .= varW16 a == varW16 b
  "init" .= [True] ++ (const False)
    where sub hi lo = mux (varW16 hi > var lo) (var hi - var lo) (var hi)
          alg ext ex = [0] ++ mux (var "init") (extW16 ext 1) ex

testCoercions :: Streams
testCoercions = do
    "short" .= ((cast (varW64 "fib"))::(Spec Word8))  
    "long" .= ((cast (varW8 "short"))::(Spec Word32))

testCoercionsInt :: Streams
testCoercionsInt = do
    "counter" .= [0] ++ varW8 "counter" + 1 
    "int" .= ((cast (varW8 "counter"))::(Spec Int8)) 

testRules :: Streams
testRules = do
    "v1" .= (not (Const True)) || Var "v2" 
    "v2" .= [True, False] ++ [True] ++ Var "v3" < extI8 "v4" 5 
    "v3" .= 0 + drop 3 (int8 6) 
    "v4" .= always 5 (Var "v1") 

i8 :: Streams
i8 = do
    v .=  [0, 1] ++ (varI8 v) + 1 
    
trap :: Streams
trap = do
    "target" .= [0] ++ varW32 "target" + 1 
    "x" .= [0,0] ++ var "y" + varW32 "target" 
    "y" .= [0,0] ++ var "x" + varW32 "target" 
    

-- vicious :: Streams
-- vicious = do 
--     "varExt" .= extW32 "ext" 5 
--     "vicious" .= [0,1,2,3] ++ drop 4 (varW32 "varExt") + drop 1 (var "varExt") + var "varExt" 

-- testVicious :: Streams
-- testVicious = do
--     "counter" .= [0] ++ varW32 "counter" + 1 
--     "testVicious" .= [0,0,0,0,0,0,0,0,0,0] ++ drop 8 (varW32 "counter") 

-- -- The issue is when a variable v with a prophecy array of length n deps on
-- -- an external variable pv with a weight w, and that w > - n + 1 Here, w = 0 and
-- -- n = 2, so 0 > -1 holds.  That means that it is impossible to fill the last
-- -- case of the prophecy array, because it is not yet known what the external
-- -- variable will be worth.  It could be easily forbidden in the analyser.
-- -- However theoretically, nothing seems to prevent us form compiling it, we
-- -- would only need a way to say that in these case the middle of the prophecy
-- -- array should be updated and not the . (it would be safe because if another
-- -- variable was to dep on the  of it it would dep on the external
-- -- variable with a weight > 0, which is always forbidden).  It is probably
-- -- easier for now to just forbid it. But it could become an issue.
-- isBugged :: Streams
-- isBugged = do
--     "v" .= extW16 "ext" 5 
--     "v2" .= [0,1,3] ++ drop 1 (varW16 "v") 
    

-- -- The next two examples are currently refused, because they include a
-- -- non-negative weighted closed path. But they could be compiled.  More
-- -- generally I think that this restriction could be partially lifted to
-- -- forbiding non-negative circuits.  However, this would demand longer
-- -- prophecyArrays than we have for now.  (and probably a slightly different
-- -- algorithm) So even if we partially lift this restriction, a warning should
-- -- stay, because it breaks the current easy-to-evaluate bound on the memory
-- -- requirement of a Copilot monitor.
-- shouldBeRight :: Streams
-- shouldBeRight = do
--     "v1" .= [0] ++ varI32 "v1" + 1 
--     "v2" .= drop 2 (varI32 "v1") 

-- shouldBeRight2 :: Streams
-- shouldBeRight2 = do
--     "loop1" .= [0] ++ varI32 "loop2" + 2 
--     "loop2" .= [1] ++ varI32 "loop1" - 1 
--     "other" .= drop 3 (varI32 "loop1") 
