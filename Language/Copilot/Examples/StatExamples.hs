module Language.Copilot.Examples.StatExamples where

import Prelude ()
import Language.Copilot.Core
import Language.Copilot.Language
import Language.Copilot.Libs.Statistics

t0 :: Streams
t0 = do
  let minV = varW16 "min"
      maxV = varW16 "max"
      sumV = varW16 "sum"
      a = varW16 "a"

  a .= [0..5] ++ a + 6
  minV .= min 3 a
  maxV .= max 3 a
  sumV .= sum 3 a

tMean :: Streams
tMean = do
  let a = varD "a"
      out = varD "out"

  a .= [0..5] ++ a + 6
  out .= mean 4 a
