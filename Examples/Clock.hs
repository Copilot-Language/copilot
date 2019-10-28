-- | Clocks library tests.

module ClockExamples ( clockExamples ) where

import Prelude ( putStrLn, IO, Bool )
import Copilot.Language
import Copilot.Library.Clocks

p :: Word8
p = 5 

clkStream, clk1Stream :: Stream Bool
clkStream  = clk  ( period p ) ( phase 0 )
clk1Stream = clk1 ( period p ) ( phase 0 )


clockTest :: Spec
clockTest = do
  observer "clk" clkStream
  observer "clk1" clk1Stream


clockExamples :: IO ()
clockExamples = do
  prettyPrint clockTest
  putStrLn ""
  putStrLn ""
  interpret 10 clockTest


main = clockExamples
