{-# LANGUAGE FlexibleContexts #-}

module Language.Copilot.Examples.Examples where

import Data.Word
import Prelude (($))
import qualified Prelude as Prelude
import qualified Prelude as P

-- for specifying options
import Data.Map (fromList) 
--import Data.Maybe (Maybe (..))
--import System.Random

import Language.Copilot 
-- import Language.Copilot.Variables

fib :: Streams
fib = do
  let f = varW64 "f"
      t   = varB "t"
  f .= [0,1] ++ f + (drop 1 f)
  t .= even f
    where even :: Spec Word64 -> Spec Bool
          even w' = w' `mod` 2 == 0

t1 :: Streams
t1 = do
  let x = varI32 "x"
      y = varB "y"
      z = varB "z"
      w = varB "w"
  x .= [0, 1, 2] ++ x - (drop 1 x)
  y .= [True, False] ++ y ^ z
  z .= x <= drop 1 x
  w .= 3 == x

-- t2 :: Streams
-- t2 = do
--      a .= [True] ++ not (var a) 
--      b .= mux (var a) 2 (int8 3) 

-- t3 :: use an external variable called ext, typed Word32
t3 :: Streams
t3 = do
  let a    = varW32 "a"
      b    = varB "b"
      ext8 = extW32 "ext"
      ext1 = extW32 "ext"
  a .= [0,1] ++ a + ext8 + ext8 + ext1
  b .= [True, False] ++ 2 + a < 5 + ext1

t4 :: Streams
t4 = do
  let a = varB "a"
      b = varB "b"
  a .= [True,False] ++ not a
  b .= drop 1 a

t5 :: Streams
t5 = do
  let x = varW16 "x"
      y = varB "y"
      z = varB "z"
  x .= [0] ++ x + 1
  y .= [True, True] ++ y
  z .= [False] ++ not z
  -- triggers
  trigger y "y_trigger" void
  trigger z "z0_trigger" x
  trigger z "z1_trigger" (y <> constW16 3 <> x)
  
yy :: Streams
yy = 
  let a = varW64 "a"
  in  do a .= 4  

zz :: Streams
zz = do
  let a = varW32 "a"
      b = varW32 "b"
  --a .= [0..4] ++ drop 4 (varW32 a) + 1
  a .= a + 1
  b .= drop 3 a

xx :: Streams
xx = do
  let a = varW32 "a"
      b = varW32 "b"
      c = varW32 "c"
      e = extW32 "ext"
      d = varB "d"
      f = extW32 (fun "fun1" void) 
      h = extArrW16 (fun "g" b) a 
      g = extW16 (fun "fun2" (true <> b <> constW16 3))
      w = varB "w"
  a .= e + f
  b .= [3] ++ a
  c .= [0, 1, 3, 4] ++ drop 1 b
  d .= h < g
  -- triggers
  trigger d "y_trigger" void
  trigger w "z0_trigger" (a <> b <> true)
  trigger w "z1_trigger" (d <> constW16 3 <> w)

-- If the temperature of the engine rises more than 2.3 degrees within 2 seconds
-- and the cooler is not engaged, trigger the shutoff function.
engineMonitor :: Streams
engineMonitor = do
  -- external vars
  let temp     = extF "temp"
      cooler   = extB "cooler"
  -- Copilot vars
      temps    = varF "temps"
      overTemp = varB "overTemp"
  temps    .= [0, 0, 0] ++ temp
  overTemp .= drop 2 temps > 2.3 + temps
  trigger overTemp "shutoff" void

-- | Sending over ports.
distrib :: Streams
distrib = do
  -- vars
  let a = varW8 "a"
      b = varB "b"
  -- spec
  a .= [0,1] ++ a + 1
  b .= mod a 2 == 0 
  -- sends
  send "portA" (port 2) a 
  send "portB" (port 1) b 

monitor :: Streams
monitor = do
  -- external variables
  let word     = extW32 "rx"
      arbiter  = extB "arbiter"
  -- Copilot variables
      words    = varW32 "words"
      arbiters = varB "arbiters"
  words   .=      [0] ++ mux arbiter word words
  arbiters .= [False] ++ mux arbiter false arbiter

-- greatest common divisor.
gcd :: Word16 -> Word16 -> Streams
gcd n0 n1 = do
  let a = varW16 "a"
      b = varW16 "b"
  a .= alg n0 a b
  b .= alg n1 b a

  let ans = varB "ans"
  ans .= a == b
    where alg x0 x1 x2 = [x0] ++ mux (x1 > x2) (x1 - x2) x1

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
  -- externals
  let n = extW16 "n"
      m = extW16 "m"
  -- copilot vars
      a = varW16 "a"
      b = varW16 "b"
      init = varB "init"
      ans  = varB "ans"
  
  a .= alg n (sub a b) init
  b .= alg m (sub b a) init
  ans .= a == b && not init
  init .= [True] ++ false

  where sub hi lo = mux (hi > lo) (hi - lo) hi
        alg ext ex init = [0] ++ mux init ext ex

testCoercions :: Streams
testCoercions = do
  let word = varW8 "word"
      int = varI16 "int"
  word .= [1] ++ word * (-2)
  int  .= 1 + cast word

testCoercions2 :: Streams
testCoercions2 = do
  let b = varB "b"
      i = varI16 "i"
      j = varI16 "j"
  b .= [True] ++ not b
  i .= cast j
  j .= 3

testCoercions3 :: Streams
testCoercions3 = do
  let x = varB "x"
      y = varI32 "y"
      z = varI32 "z"
  x .= [True, True] ++ not x
  y .= cast x + cast x
--  z .= drop 1 (cast x)
  z .= cast (not true)

i8 :: Streams
i8 = let v = varI8 "v" in v .= [0, 1] ++ v + 1 
    
trap :: Streams
trap = do
  let target = varW32 "target"
  target .= [0] ++ target + 1 

  let x = varW32 "x"
  let y = varW32 "y"
  x .= [0,0] ++ y + target
  y .= [0,0] ++ x + target

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

-- testing external array references
testArr :: Streams
testArr = do 
  -- a .= [True] ++ extArrB ("ff", varW16 b) 5 && extArrB ("ff", varW16 b) 1 
  --       && extArrB ("ff", varW16 b) 2
  -- b .= [7] ++ varW16 b + 3 + extArrW16 ("gg", varW16 f) 2 
  -- b .= [0] ++ extArrW16 ("gg", varW16 b) 4 
  -- c .= [True] ++ var c
  -- d .= varB c 
  let e = varW16 "e"
  e .= [6,7,8] ++ 3 -- + extArrW16 ("gg", varW16 b) 2
--  f .= extArrW16 ("gg", varW16 e) 2 + extArrW16 ("gg", varW16 e) 2 
  let g = varB "g"
  let gg = extArrW16 "gg" e 
  g .= gg < 13
  -- h .= [0] ++ drop 1 (varW16 g)


-- t3 :: use an external variable called ext, typed Word32
t99 :: Streams
t99 = do
  let ext = extW32 "ext"
      a = varW32 "a"
  a .= [0,1] ++ a + ext + ext + ext 

  let b = varB "b"
  b .= [True, False] ++ 2 + a < 5 + ext 

t11 :: Streams
t11 = do
  let v = varB "v"
  v .= [False, True, False] ++ v

tdiv :: Streams
tdiv = do
  let a = varW16 "a"
      b = varW16 "b"
  a .= [3] ++ a `div` 5
  b .= [3] ++ a `mod` 5

extT :: Streams
extT = do
  let x = extW16 "x"
  let y = varW16 "y"
  y .= x 

-- Examples:
interpretExtT :: Prelude.IO ()
interpretExtT =
  interpret extT 10 $ 
    setE (emptySM {w16Map = fromList [("x", [8,9..])]})
      baseOpts
compileExtT :: P.IO ()
compileExtT =
  compile extT "extT" $ setCode (P.Just "#include \"mylib.h\"", P.Nothing) baseOpts

-- Testing error checking for badly-formed external sampling.

-- Should fail.
extT2 :: Streams
extT2 = do
  let y = varW16 "y"
      x = extW16 (fun "f" y)
  y .= x 

-- Should pass.
extT3 :: Streams
extT3 = do
  let y = varW16 "y"
      x = extW16 (fun "f" y)
  y .= [3] ++ x 

-- Should fail.
extT4 :: Streams
extT4 = do
  let y = varW16 "y"
      x = extArrW16 "arr" y
      z = extW16 "z"
      w = varW16 "w"
  y .= z
  w .= x

-- Should fail.
extT5 :: Streams
extT5 = do
  let y = varW16 "y"
      x = extW16 (fun "f" y)
      z = extW16 "z"
      w = varW16 "w"
  y .= z
  w .= x

-- Should work.
extT6 :: Streams
extT6 = do
  let y = varW16 "y"
      x = extArrW16 "arr" y
      z = extW16 "z"
      w = varW16 "w"
  y .= [7] ++ z
  w .= x



-- test external idx before after and in the stream it references
-- test multiple defs
-- test defs in functions
-- test arrays at different indexes
