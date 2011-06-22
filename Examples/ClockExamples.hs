-- | Clocks library tests.

module Copilot.Examples.ClockExamples where


import Prelude ( ($), putStrLn )
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
  observer "clk" clkStream
  observer "clk1" clk1Stream


test = do
  prettyPrint clockTest
  putStrLn ""
  putStrLn ""
  interpret 10 [] clockTest
