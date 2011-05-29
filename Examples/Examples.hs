-- |

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module Examples where

import qualified Prelude as P
import "copilot-language" Copilot.Language.Prelude hiding (even)
import "copilot-language" Copilot.Language

-- The sequence of natural numbers:
nats ∷ Stream Word64
nats = [0] ++ (1 + nats)

-- The Fibonacci sequence:
fib ∷ Stream Word64
fib = [1, 1] ++ fib + drop 1 fib

-- A 'pure' function on streams, in the sense that in contains
-- no internal state:
--even ∷ (Streamable α, Integral α)  ⇒ Stream α → Stream Bool
--even x = x `mod` 2 == 0

-- The CoPilot equivalent of a boolean flipflop.
flipflop ∷ Stream Bool → Stream Bool
flipflop x = y
  where
    y = [False] ++ mux x (not y) y

-- A resetable counter.
counter ∷ (Num α, Streamable α) ⇒ Stream Bool → Stream Bool → Stream α
counter tick reset = y
  where
    zy = [0] ++ y
    y  = mux reset 0 $
         mux tick (zy + 1) $
         zy

-- An alarm.
someAlarm ∷ Int32 → Stream Bool → Stream Bool → Stream Bool → Stream Bool
someAlarm limit order done tick = alarm
  where
    running = mux order true $
              mux done  false $
              mux ([False] ++ alarm) false $
              [False] ++ running
    count   = counter (tick && running) (order || done)
    alarm   = count > constant limit

x ∷ Stream Bool
x = true

y ∷ Stream Bool
y = [False, False, False, True] ++ y

-- We can use (&&), (||), (==), (++), etc., both on streams
-- and ordinary Haskell values:

imply :: Bool -> Bool -> Bool
imply p q = not p || q

fibList :: [Integer]
fibList = [1, 1] ++ zipWith (+) fibList (drop 1 fibList)

spec :: Specification
spec =
  [
    trigger "fib"      x fib
  ,
    trigger "nats"     y nats
  ,
    trigger "counter"  x (counter x y :: Stream Int64)
  ,
    trigger "booleans" x y
  ]

main ∷ IO ()
main =
  do
    interpret 100 spec
    prettyPrint spec
