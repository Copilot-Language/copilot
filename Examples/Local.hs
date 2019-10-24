--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Example demonstrating local variables.

module Local ( localEx ) where

import Prelude ()
import Language.Copilot

--------------------------------------------------------------------------------

nats :: Stream Int32
nats = [0] ++ (1 + nats)

strm :: Stream Int32
strm =
  local (nats + 1) $ \nats' -> nats' + nats'

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

localEx :: IO ()
localEx = do
  interpret 20 spec
  prettyPrint spec

--------------------------------------------------------------------------------

main = localEx
