{-# LANGUAGE FlexibleContexts #-}

module Language.Copilot.Examples.PTLTLExamples where

import Prelude ()

import Language.Copilot.Core
import Language.Copilot.Language
import Language.Copilot.Interface 
import Language.Copilot.Variables
import Language.Copilot.PrettyPrinter
import Language.Copilot.Libs.PTLTL

-- Next examples are for testing of the ptLTL library

-- test of previous
tstdatprv :: Streams
tstdatprv = a .= [True, False] ++ varB a 
               
tprv :: Streams
tprv = do
  tstdatprv 
  c `previous` (varB a) 

-- test of alwaysBeen
tstdatAB ::  Streams
tstdatAB = d .= [True, True, True, True, True, True, True, False] ++ varB d

tAB :: Streams
tAB = do
   tstdatAB 
   f `alwaysBeen` (var d) 

-- test of eventuallyPrev
tstdatEP ::  Streams
tstdatEP = g .= [False, False, False, False, False, True, False] ++ varB g 

tEP :: Streams
tEP = do
    tstdatEP 
    h `eventuallyPrev` (varB g) 

-- test of since
tstdat1Sin :: Streams
tstdat1Sin = "q1" .= [True, True, True, True, True, True, False ] ++ varB "q1"

tstdat2Sin :: Streams
tstdat2Sin = "q2" .= [False, False, True, False, False, False, False ] ++ varB "q2"
                  
tSin :: Streams 
tSin = do 
    tstdat1Sin 
    tstdat2Sin 
    "z1" `since` (varB "q1", varB "q2") 

-- with external variables
-- interface $ setE (emptySM {bMap = fromList [("e1", [True,True ..]), ("e2", [False,False ..])]}) $ interpretOpts tSinExt 20
tSinExt :: Streams 
tSinExt = do
      "z1" `since` (extB "e1" 2, extB "q2" 3) 
