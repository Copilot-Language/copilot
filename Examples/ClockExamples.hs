-- | Clocks library tests.

module Copilot.Examples.ClockExamples where

import Prelude ( ($) )
import qualified Prelude as P
import Copilot.Language
import Copilot.Library.Clocks
import Data.Bool
import Data.List ( replicate )


p = 5 :: Word8


clkStream  = clk  ( period p ) ( phase 0 )
clk1Stream = clk1 ( period p ) ( phase 0 )


clockTest :: Spec
clockTest = do
  trigger "clk"  true [ arg $ clkStream  ]
  trigger "clk1" true [ arg $ clk1Stream ]


test = do
  prettyPrint clockTest
  interpret 10 [] clockTest
