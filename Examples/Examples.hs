-- |


module Examples where

import Prelude ()
import Copilot.Language
import Copilot.Language.Prelude hiding (even)
import Copilot.Language.Reify (reify)
import Copilot.Compile.C99 (compile)

-- The sequence of natural numbers:
nats :: Stream Word64
nats = [0] ++ (1 + nats)

-- The Fibonacci sequence:
fib :: Stream Word64
fib = [1, 1] ++ fib + drop 1 fib

-- A 'pure' function on streams, in the sense that in contains
-- no internal state:
--even :: (Streamable α, Integral α) => Stream α -> Stream Bool
--even x = x `mod` 2 == 0

-- The Copilot equivalent of a boolean flipflop.
flipflop :: Stream Bool -> Stream Bool
flipflop x = y
  where
    y = [False] ++ mux x (not y) y

-- A resetable counter.
counter :: (Num α, Typed α, Show α) => Stream Bool -> Stream Bool -> Stream α
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
    alarm   = count > constant limit

x :: Stream Bool
x = true

y :: Stream Bool
y = [True, True, False] ++ y

-- We can use (&&), (||), (==), (++), etc., both on streams
-- and ordinary Haskell values:

imply :: Bool -> Bool -> Bool
imply p q = not p || q

fibList :: [Integer]
fibList = [1, 1] ++ zipWith (+) fibList (drop 1 fibList)

spec :: [Trigger]
spec =
  [
    trigger "trigger" y
      [ stream $ (counter x (drop 1 y) :: Stream Word64)
-- , stream $ (extern "period" :: Stream Word64)
      , stream $ fib
      ]
  ]

main :: IO ()
main = reify spec >>= compile "test"
