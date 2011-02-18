{-# LANGUAGE FlexibleContexts #-}

module Language.Copilot.Examples.PTLTLExamples where

import Prelude (($), repeat, replicate, IO())
import qualified Prelude as P
import Data.Map (fromList)

import Language.Copilot
import Language.Copilot.Libs.PTLTL
import Language.Copilot.Libs.Vote

-- Next examples are for testing of the ptLTL library

-- test of previous
tstdatprv :: Streams
tstdatprv = do
  let a = varB "a"
  a .= [True, False] ++ a 
               
tprv :: Streams
tprv = do
  let c = varB "c"
      a = varB "a"
  tstdatprv 
  c `ptltl` previous a

-- test of alwaysBeen
tstdatAB ::  Streams
tstdatAB = do
  let d = varB "d"
  d .= [True, True, True, True, True, True, True, False] ++ d

tAB :: Streams
tAB = do
   let f = varB "f"
       d = varB "d"
   tstdatAB 
   f `ptltl` alwaysBeen d

-- test of eventuallyPrev
tstdatEP ::  Streams
tstdatEP = do
  let g = varB "g"
  g .= [False, False, False, False, False, True, False] ++ g 

tEP :: Streams
tEP = do
    let h = varB "h"
        g = varB "g"
    tstdatEP 
    h `ptltl` eventuallyPrev g

q1, q2, z :: Spec Bool
q1 = varB "q1"
q2 = varB "q2"
z = varB "z"

-- test of since
tstdat1Sin :: Streams
tstdat1Sin = q1 .= [False, False, False, False, True] ++ true --varB "q1"

tstdat2Sin :: Streams
tstdat2Sin = q2 .= [False, False, True, False, False, False, False ] ++ q2
                  
tSince :: Streams 
tSince = do
    tstdat1Sin 
    tstdat2Sin 
    z `ptltl` (q1 `since` q2)

-- with external variables
-- interface $ setE (emptySM {bMap = fromList [("e1", [True,True ..]), ("e2", [False,False ..])]}) $ interpretOpts tSinExt 20
tSinExt :: Streams 
tSinExt = do
  let e1 = extB "e1"
      e2 = extB "e2"
  z `ptltl` (e1 `since` e2)

tSinExt2 :: Streams 
tSinExt2 = do
  let e1 = extB "e1"
      e2 = extB "e2"
      a = varB "a"
      s = varB "s"
      d = varB "d"
      t = varB "t"
      e = varB "e"

  a .= not e1
  s `ptltl` (e2 `since` d)
  t `ptltl` (alwaysBeen $ not a)
  e .= e1 ==> d

-- "If the majority of the engine temperature probes exeeds 250 degrees, then
-- the cooler is engaged and remains engaged until the majority of the engine
-- temperature drops to 250 or below.  Otherwise, trigger an immediate shutdown
-- of the engine."
engine :: Streams
engine = do
  -- external vars
  let t0       = extW8 "temp_probe_0"
      t1       = extW8 "temp_probe_1"
      t2       = extW8 "temp_probe_2"
      cooler   = extB  "fan_status"
  -- Copilot vars
      maj      = varW8 "maj"
      check    = varB  "maj_check"
      overHeat = varB  "over_heat"
      monitor  = varB  "monitor"
  -- local vars
      temps    = [t0, t1, t2]
  maj      .= majority temps
  check    .= aMajority temps maj
  overHeat `ptltl` (        (cooler || (maj <= 250 && check)) 
                    `since` (maj > 250))
  monitor .= not overHeat
  trigger monitor "shutoff_engine" void


engineRun :: Bool -> IO ()
engineRun b = if b then 
  interpret engine 40 $ 
    setE (emptySM { bMap  = fromList [("fan_status", replicate 3 False P.++ repeat False)]
                  , w8Map = fromList [ ("temp_probe_0", [240,240..])
                                     , ("temp_probe_1", [240,241..])
                                     , ("temp_probe_2", [240,241..])
                                     ]
                  }) 
    baseOpts
  else compile engine "engine" $ setSim baseOpts

