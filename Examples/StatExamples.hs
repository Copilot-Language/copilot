module StatExamples ( statExamples ) where

-- | Statistics examples

import Prelude ()
import Language.Copilot

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
  observer "minV"  minV
  observer "maxV"  maxV
  observer "sumV"  sumV
  observer "meanV" meanV


statExamples :: IO ()
statExamples = do
  prettyPrint statisticsTest
  putStrLn ""
  putStrLn ""
  interpret 20 statisticsTest

main = statExamples
