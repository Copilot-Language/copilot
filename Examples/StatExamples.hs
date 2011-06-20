module Copilot.Examples.StatExamples where


import Prelude ()
import Copilot.Language
import Copilot.Language.Prelude hiding ( min, max, sum )
import Copilot.Library.Statistics


inputData :: Stream Word16
inputData = replicate 5 0 ++ inputData + 5

inputFData :: Stream Float
inputFData = replicate 5 0 ++ inputFData + 5


minV :: Stream Word16
minV = min 3 inputData

maxV :: Stream Word16
maxV = max 3 inputData

sumV :: Stream Word16
sumV = sum 3 inputData

meanV :: Stream Float
meanV = mean 3 inputFData


statisticsTest :: Spec
statisticsTest = do
  trigger "minV"  true [ arg $ minV  ]
  trigger "maxV"  true [ arg $ maxV  ]
  trigger "sumV"  true [ arg $ sumV  ]
  trigger "meanV" true [ arg $ meanV ]



test = do
  prettyPrint statisticsTest
  putStrLn ""
  putStrLn ""
  interpret 20 [] statisticsTest
