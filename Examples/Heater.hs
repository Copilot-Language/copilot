--------------------------------------------------------------------------------
-- Copyright © 2019 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- This is a simple example with basic usage. It implements a simple home
-- heating system: It heats when temp gets too low, and stops when it is high
-- enough. It read temperature as an byte (range -50C to 100C) and translates
-- this to Celcius.

module Heater where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div)

-- External temperature as a byte, range of -50C to 100C
temp :: Stream Int8
temp = extern "temperature" Nothing

-- Calculate temperature in Celcius.
-- We need to cast the Int8 to a Float. Note that it is an unsafeCast, as there
-- is no direct relation between Int8 and Float.
ctemp :: Stream Float
ctemp = ((unsafeCast temp) / 150.0) - 50.0

spec = do
  -- Triggers that fire when the ctemp is too low or too hight,
  -- pass the current ctemp as an argument.
  trigger "heaton"  (ctemp < 18.0) [arg ctemp]
  trigger "heafoff" (ctemp > 21.0) [arg ctemp]

-- Compile the spec
main = reify spec >>= compile "heater"
