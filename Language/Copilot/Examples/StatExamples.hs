module Language.Copilot.Examples.StatExamples where

import Prelude ()
import Language.Copilot.Core
import Language.Copilot.Language
import Language.Copilot.Interface
import Language.Copilot.Variables
import Language.Copilot.PrettyPrinter
import Language.Copilot.Libs.Statistics

t0 :: Streams
t0 = do
  a .= [0..5] ++ (varW16 a) + 6
  "min" .= min 3 (varW16 a)
  "max" .= max 3 (varW16 a)
  "sum" .= sum 3 (varW16 a)

tMean :: Streams
tMean = do
  a .= [0..5] ++ (varD a) + 6
  "out" .= mean 4 (varD a)
