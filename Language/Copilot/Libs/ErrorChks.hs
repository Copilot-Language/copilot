-- | Basic error checking for other libraries.

module Language.Copilot.Libs.ErrorChks(nOneChk, nPosChk, int16Chk) where

import Prelude (Integral, Int, String, error, ($), show, maxBound, toInteger)
import qualified Prelude as P 
import Data.Int (Int16)

import Language.Copilot.Core

chk :: String -> Int -> Spec a -> Int -> Spec a
chk name n s m = 
  if n P.< m 
    then error $ "Value " P.++ show n P.++ " in operator " P.++ name 
         P.++ " must be greater than or equal to " P.++ show m P.++ "."
    else s

nPosChk :: String -> Int -> Spec a -> Spec a
nPosChk name n s = chk name n s 0

nOneChk :: String -> Int -> Spec a -> Spec a
nOneChk name n s = chk name n s 1

int16Chk :: String -> Int -> Spec a -> Spec a
int16Chk name n s = 
  if (toInteger n) P.> max
    then error $ "Offset " P.++ show n P.++ " in operator " P.++ name 
         P.++ " is larger than the Int16 maxBound, " P.++ show max P.++ "."
    else s
  where max = toInteger (maxBound::Int16)
