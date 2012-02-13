--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Some Copilot examples.

{-# LANGUAGE RebindableSyntax #-}

module Examples ( examples ) where

import qualified Prelude as P
import Language.Copilot hiding (even, odd)
--import Copilot.Compile.C99
import qualified Copilot.Tools.CBMC as C

--------------------------------------------------------------------------------

--
-- Some utility functions:
--

{-

implyStream :: Stream Bool -> Stream Bool -> Stream Bool
implyStream p q = not p || q


extEven :: Stream Bool
extEven = externX `mod` 2 == 0

oddSpec :: Spec
oddSpec = trigger "f" true [arg (odd nats)]

prop :: Stream Bool
prop = (x - x') <= 5 && (x - x') <= (-5)
  where
  x :: Stream Int32
  x  = [0] ++ cast externX
  x' = drop 1 x

externX :: Stream Int8
externX = extern "x" (Just [0..])

foo :: Spec
foo = do
  let x = cast externX :: Stream Int16
  trigger "trigger" true [arg $ x < 3]
  observer "debug_x" x

latch :: Stream Bool -> Stream Bool
latch x = out
  where out = if x then not st else st
        st  = [False] ++ out

latch' :: Stream Bool -> Stream Bool
latch' x = out
  where out = x `xor` st
        st  = [False] ++ out

ext :: Stream Word8
ext = [1] ++ ext + extern "e0" (Just [2,4..])

-}

flipflop :: Stream Bool -> Stream Bool
flipflop x = y
  where
    y = [False] ++ if x then not y else y

nats :: Stream Word64
nats = [0] ++ nats + 1

even :: (P.Integral a, Typed a) => Stream a -> Stream Bool
even x = x `mod` 2 == 0

odd :: (P.Integral a, Typed a) => Stream a -> Stream Bool
odd = not . even

counter :: (Eq a, Num a, Typed a) => Stream Bool -> Stream a
counter reset = y
  where
  zy = [0] ++ y
  y  = if reset then 0 else zy + 1

booleans :: Stream Bool
booleans = [True, True, False] ++ booleans

fib :: Stream Word64
fib = [1, 1] ++ fib + drop 1 fib

bitWise :: Stream Word8
bitWise = ( let a = [ 1, 1, 0 ] ++ a in a )
          .^.
          ( let b = [ 0, 1, 1 ] ++ b in b )

sumExterns :: Stream Word64
sumExterns =
  let ex1 = extern "e1" (Just e1)
      ex2 = extern "e2" (Just e2)
  in  ex1 + ex2

--- Some infinite lists for simulating external variables:
e1, e2 :: [Word64]
e1 = [0..]
e2 = 5 : 4 : e2

--------------------------------------------------------------------------------

--
-- An example of a complete copilot specification.
--

-- A specification:
spec :: Spec 
spec = do

    -- A trigger with four arguments:
    trigger "e" true -- booleans
      [ arg fib, arg nats, arg sumExterns, arg bitWise ]

    -- A trigger with two arguments:
    trigger "f" booleans
      [ arg fib, arg sumExterns ]
--      [ arg fib, arg nats ]

    -- A trigger with a single argument:
    trigger "g" (flipflop booleans)
      [ arg (sumExterns + counter false + 25) ]
--      [ arg (counter false + 25 :: Stream Int32) ]

    -- A trigger with a single argument (should never fire):
    let e3 = [1, 1] P.++ zipWith (+) e3 (P.drop 1 e3)
    trigger "h" (extern "e3" (Just e3) /= fib)
      [ arg (0 :: Stream Int8) ]

    observer "i" (odd nats)

examples :: IO ()
examples = do
  putStrLn "PrettyPrinter:"
  putStrLn ""
  prettyPrint spec
  putStrLn ""
  putStrLn ""
  putStrLn "Interpreter:"
  putStrLn ""
  interpret 10 spec
  -- putStrLn ""
  -- putStrLn ""
  -- putStrLn "Atom:"
  -- reify spec >>= compile defaultParams 
  putStrLn "Check equivalence:"
  putStrLn ""
  putStrLn ""
  reify spec >>= 
    C.genCBMC C.defaultParams {C.numIterations = 20}

--------------------------------------------------------------------------------
