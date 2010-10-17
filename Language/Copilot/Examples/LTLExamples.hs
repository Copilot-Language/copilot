{-# LANGUAGE FlexibleContexts #-}

module Language.Copilot.Examples.LTLExamples () where

import Prelude (replicate, Int)

import Language.Copilot
import Language.Copilot.Variables

----------------
-- LTL tests ---
----------------

testing, output :: Var
testing = "test"
output = "output"


-- Can be tested with various values of n.  Use  the interpreter to see the outputs.

tSoonest :: Int -> Streams
tSoonest n = do testing .= (replicate (n+2) False) ++ const True
                output .= soonest n (var testing)
           

tLatest :: Int -> Streams
tLatest n = do testing .= (replicate (n-2) False) ++ const True
               output .= latest n (var testing)
          

tAlways :: Int -> Streams
tAlways n = do testing .= (replicate (n+3) True) ++ const False
               output .= always n (varB testing)
          

tFuture :: Int -> Streams
tFuture n = do testing .= (replicate (n+3) False) ++ varB a
               a .= [False] ++ not (var a)
               output .= eventually n (varB testing)
          

tUntil :: Int -> Streams
tUntil n = do "t1" .= (replicate (n+2) False) ++ varB c
              c .= [True] ++ not (var c)
              "t0" .= const True
              output .= until n (varB "t0") (varB "t1")
          

tRelease0 :: Int -> Streams
tRelease0 n = do output .= release n (const False) (const True)
            

tRelease1 :: Int -> Streams
tRelease1 n = do "t1" .= (replicate (n+2) True) ++ varB c
                 c .= [False] ++ not (var c)
                 "t0" .= const True
                 output .= release n (varB "t0") (varB "t1")
          
