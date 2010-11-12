{-# LANGUAGE FlexibleContexts #-}

module Language.Copilot.Examples.PTLTLExamples where

import Prelude (($), String, repeat, replicate, IO())
import qualified Prelude as P
import Data.Map (fromList)

import Language.Copilot
import Language.Copilot.Libs.PTLTL

-- Next examples are for testing of the ptLTL library

-- test of previous
tstdatprv :: Streams
tstdatprv = do
  let a = varB "a"
  a .= [True, False] ++ a 
               
tprv :: Streams
tprv = do
  let c = varB "c"
  let a = varB "a"
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
   let d = varB "d"
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
    let g = varB "g"
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
  let e1 = extB "e1" 2
  let e2 = extB "e2" 3
  z `ptltl` (e1 `since` e2)

tSinExt2 :: Streams 
tSinExt2 = do
  let e1 = extB "e1" 2
  let e2 = extB "e2" 2
  let a = varB "a"
  let s = varB "s"
  let d = varB "d"
  let t = varB "t"
  let e = varB "e"

  a .= not e1
  s `ptltl` (e2 `since` d)
  t `ptltl` (alwaysBeen $ not a)
  e .= e1 ==> d

-- "If the engine temperature exeeds 250 degrees, then the engine is shutoff
-- within the next 10 periods, and in the period following the shutoff, the
-- cooler is engaged and remains engaged."
engine :: Streams
engine = do
  -- external vars
  let engineTemp = extW8 "engineTemp" 1
  let engineOff  = extB "engineOff" 1
  let coolerOn   = extB "coolerOn" 1
  -- Copilot vars
  let cnt        = varW8 "cnt"
  let temp       = varB "temp"
  let cooler     = varB "cooler"
  let off        = varB "off"
  let monitor    = varB "monitor"
  temp    `ptltl` (alwaysBeen (engineTemp > 250))
  cnt     .=      [0] ++ mux (temp && cnt < 10) (cnt + 1) cnt
  off     .=      cnt >= 10 ==> engineOff
  cooler  `ptltl` (coolerOn `since` engineOff)
  monitor .=      off && cooler

engineRun :: IO ()
engineRun = 
  interpret engine 40 $ 
    setE (emptySM { bMap = fromList 
                            [ ("engineOff", replicate 8 False P.++ repeat True)
                            , ("coolerOn", replicate 9 False P.++ repeat True)
                            ]
                  , w8Map = fromList [("engineTemp", [99,100..])]
                  }) baseOpts

