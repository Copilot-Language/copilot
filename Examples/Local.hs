--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Example demonstrating local variables.

module Main where

import qualified Prelude as P
import Copilot.Language
import Copilot.Language.Prelude
import Data.List (replicate, foldl')

--------------------------------------------------------------------------------

nats :: Stream Int32
nats = [0] ++ (1 + nats)

strm :: Stream Int32
strm =
  local (nats + 1) $ \nats' -> nats' + nats'

replStrm :: Int -> Stream Int32
replStrm i =
  local (nats + 1) $ \nats' -> plus nats'
  where 
  plus :: Stream Int32 -> Stream Int32
  plus nats' = foldl' (+) 0 (replicate i nats')

replStrm_ :: Int -> Int -> Stream Int32
replStrm_ i j =
  local (replStrm i) $ \sum -> plus sum
  where 
  plus :: Stream Int32 -> Stream Int32
  plus sum = foldl' (+) 0 (replicate j sum)

-- The above code corresponds to
--
-- strm :: Stream Int32
-- strm =
--   let x = nats * nats
--   in x + x

spec :: Spec
spec = do
  trigger "strm" true [arg strm]
--  trigger "strm" true [arg $ replStrm 100000]
--  trigger "strm" true [arg $ replStrm_ 100000 10000]
  -- observer "nats" nats
  -- observer "strm" strm

--------------------------------------------------------------------------------

main :: IO ()
main = do
  interpret 20 [] spec
  prettyPrint spec

--------------------------------------------------------------------------------
