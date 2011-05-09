-- |

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}

module Examples where

import qualified Prelude as P
import "copilot-language" Language.Copilot.Prelude hiding (even)
import "copilot-language" Language.Copilot.Streamable
import "copilot-language" Language.Copilot.Interface
import Data.Int

even :: (Streamable a, Integral a) => Stream a -> Stream Bool
even x = x `mod` 2 == 0

fib :: Stream Int64
fib = [1, 1] ++ fib + drop 1 fib

flipflop :: Stream Bool -> Stream Bool
flipflop x = y
  where
    y = [False] ++ mux x (not y) y

counter :: Stream Bool -> Stream Bool -> Stream Int32
counter tick reset = y
  where
    zy = [0] ++ y
    y  = mux reset 0 $
         mux tick (zy + 1) $
         zy

someAlarm :: Int32 -> Stream Bool -> Stream Bool -> Stream Bool -> Stream Bool
someAlarm limit order done tick = alarm
  where
    running = mux order true $
              mux done  false $
              mux ([False] ++ alarm) false $
              [False] ++ running
    count   = counter (tick && running) (order || done)
    alarm   = count > const limit

nats :: Stream Int32
nats = [0] ++ (1 + nats)

arr1 :: Stream (Array Int32)
arr1 = array [1,2,3,4,5]

arr2 :: Stream (Array Int32)
arr2 = array [5,6,7,8,9]

arr3 :: Stream (Array Int32)
arr3 = array [0,1,2,3,4,5,6,7,8,9]

asum :: Stream (Array Int32)
asum = arr1 + arr2

fetch :: Stream Int32
fetch = asum !! (3 :: Stream Int32)

fft :: Stream Bool
fft = [False, False, True] ++ fft

main :: IO ()
main = execute 1000 (even fib == fft)
