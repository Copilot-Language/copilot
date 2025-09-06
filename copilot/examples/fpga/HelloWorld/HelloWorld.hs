module Main where

import Language.Copilot
import Copilot.Compile.Bluespec

-- Define switches input
sw :: Stream Word8
sw = extern "sw" Nothing

-- Define LEDS output
leds :: Stream Word8
leds = sw

spec = do
  trigger "leds" true [arg leds]

main = do
  spec' <- reify spec
  compile "HelloWorld" spec'
