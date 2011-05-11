-- |

{-# LANGUAGE PackageImports #-}

module Examples where

import qualified Prelude as P
import "copilot-language" Language.Copilot.Interface.Prelude hiding (even)
import "copilot-language" Language.Copilot.Interface

-- The sequence of natural numbers:
nats :: Stream Word64
nats = [0] ++ (1 + nats)

-- The Fibonacci sequence:
fib :: Stream Word64
fib = [1, 1] ++ fib + drop 1 fib

-- A 'pure' function on streams, in the sense that in contains
-- no internal state:
even :: (Streamable a, Integral a) => Stream a -> Stream Bool
even x = x `mod` 2 == 0

-- The CoPilot equivalent of a boolean flipflop.
flipflop :: Stream Bool -> Stream Bool
flipflop x = y
  where
    y = [False] ++ mux x (not y) y

-- A resetable counter.
counter :: Stream Bool -> Stream Bool -> Stream Int32
counter tick reset = y
  where
    zy = [0] ++ y
    y  = mux reset 0 $
         mux tick (zy + 1) $
         zy

-- An alarm.
someAlarm :: Int32 -> Stream Bool -> Stream Bool -> Stream Bool -> Stream Bool
someAlarm limit order done tick = alarm
  where
    running = mux order true $
              mux done  false $
              mux ([False] ++ alarm) false $
              [False] ++ running
    count   = counter (tick && running) (order || done)
    alarm   = count > const limit

-- Demonstrates that the interpreter doesn't explode
-- exponentially when evaluating the Fibonacci sequence,
-- by skipping the first 10000 numbers:
test0 :: Stream Word64
test0 = drop 1000000 fib

main :: IO ()
main = interpret 10 test0
