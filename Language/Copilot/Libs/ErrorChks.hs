-- | Basic error checking for other libraries.

module Language.Copilot.Libs.ErrorChks(nOneChk, nPosChk, int16Chk) where

import Data.Int (Int16)

import Language.Copilot.Core

chk :: String -> Int -> Spec a -> Int -> Spec a
chk name n s m = 
  if n < m 
    then error $ "Value " ++ show n ++ " in operator " ++ name 
         ++ " must be greater than or equal to " ++ show m ++ "."
    else s

nPosChk :: String -> Int -> Spec a -> Spec a
nPosChk name n s = chk name n s 0

nOneChk :: String -> Int -> Spec a -> Spec a
nOneChk name n s = chk name n s 1

int16Chk :: String -> Int -> Spec a -> Spec a
int16Chk name n s = 
  if (toInteger n) > maxVal
    then error $ "Offset " ++ show n ++ " in operator " ++ name 
         ++ " is larger than the Int16 maxBound, " ++ show maxVal ++ "."
    else s
  where maxVal = toInteger (maxBound::Int16)
