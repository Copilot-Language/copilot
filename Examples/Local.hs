--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Example demonstrating local variables.

module Main where

import qualified Prelude as P
import Copilot.Language
import Copilot.Language.Prelude

--------------------------------------------------------------------------------

nats :: Stream Int32
nats = [0] ++ (1 + nats)

strm :: Stream Int32
strm =
  local "x" (nats * nats) $
    var "x" + var "x"

-- The above code corresponds to
--
-- strm :: Stream Int32
-- strm =
--   let x = nats * nats
--   in x + x

spec :: Spec
spec =
  do
    observer "nats" nats
    observer "strm" strm

--------------------------------------------------------------------------------

main :: IO ()
main =
  do
    interpret 20 [] spec
    prettyPrint spec

--------------------------------------------------------------------------------