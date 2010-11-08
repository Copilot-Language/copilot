{-# LANGUAGE FlexibleContexts #-}

module Language.Copilot.Examples.PTLTLExamples where

import Prelude (($), String, repeat, replicate, IO())
import qualified Prelude as P
import Data.Map (fromList)

import Language.Copilot
import Language.Copilot.Libs.PTLTL
import Language.Copilot.Variables

-- Next examples are for testing of the ptLTL library

-- test of previous
tstdatprv :: Streams
tstdatprv = a .= [True, False] ++ varB a 
               
tprv :: Streams
tprv = do
  tstdatprv 
  c `ptltl` (previous $ varB a)

-- test of alwaysBeen
tstdatAB ::  Streams
tstdatAB = d .= [True, True, True, True, True, True, True, False] ++ varB d

tAB :: Streams
tAB = do
   tstdatAB 
   f `ptltl` (alwaysBeen $ var d)

-- test of eventuallyPrev
tstdatEP ::  Streams
tstdatEP = g .= [False, False, False, False, False, True, False] ++ varB g 

tEP :: Streams
tEP = do
    tstdatEP 
    h `ptltl` (eventuallyPrev $ varB g)

-- test of since
tstdat1Sin :: Streams
tstdat1Sin = "q1" .= [False, False, False, False, True] ++ true --varB "q1"

tstdat2Sin :: Streams
tstdat2Sin = "q2" .= [False, False, True, False, False, False, False ] ++ varB "q2"
                  
tSince :: Streams 
tSince = do
    tstdat1Sin 
    tstdat2Sin 
    "z1" `ptltl` (varB "q1" `since` varB "q2")

-- with external variables
-- interface $ setE (emptySM {bMap = fromList [("e1", [True,True ..]), ("e2", [False,False ..])]}) $ interpretOpts tSinExt 20
tSinExt :: Streams 
tSinExt = do
  ptltl "z1" $ extB "e1" 2 `since`extB "q2" 3

tSinExt2 :: Streams 
tSinExt2 = do
  a .= not (extB "e1" 2)
  b .= constW16 3 > 2
  s `ptltl` ((extB "e2" 2) `since` (var b))
  t `ptltl` (alwaysBeen $ not (varB a))
  e .= extB "e1" 2 ==> varB d


engineTemp, engineOff, coolerOn, monitor, temp, cooler, off, cnt :: String
engineTemp = "engineTemp"
engineOff  = "engineOff"
coolerOn   = "coolerOn"
monitor    = "trigger"
temp       = "temp"
off        = "off"
cnt        = "cnt"
cooler     = "cooler"

-- "If the engine temperature exeeds 250 degrees, then the engine is shutoff
-- within the next 10 periods, and in the period following the shutoff, the
-- cooler is engaged and remains engaged."
engine :: Streams
engine = do
  temp    `ptltl` alwaysBeen (extW8 engineTemp 1 > 250)
  cnt     .=      [0] ++ mux (varB temp && varW8 cnt < 10) 
                             (varW8 cnt + 1)
                             (varW8 cnt)
  off     .=      (varW8 cnt >= 10 ==> extB engineOff 1)
  cooler  `ptltl` (extB coolerOn 1 `since` extB engineOff 1)
  monitor .=      varB off && varB cooler

engineRun :: IO ()
engineRun = 
  interpret engine 40 $ 
    setE (emptySM { bMap = fromList 
                            [ (engineOff, replicate 8 False P.++ repeat True)
                            , (coolerOn, replicate 9 False P.++ repeat True)
                            ]
                  , w8Map = fromList [(engineTemp, [99,100..])]
                  }) baseOpts

