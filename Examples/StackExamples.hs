-- | Stack library tests.

module StackExamples ( stackExamples ) where


import Prelude ()
import qualified Prelude as P
import Copilot.Language
import Copilot.Library.Stacks
import Copilot.Language.Prelude


-- push a counter from 1 to 5 onto the stack
pushSignal :: Stream Bool
pushSignal = replicate 5 True ++ false

pushValue :: Stream Word16
pushValue  = [ 1 ] ++ pushValue + 1

-- then wait one tick and pop 6 values off the stack again
-- ( leaving the default/start value )
popSignal :: Stream Bool
popSignal = replicate 6 False P.++ replicate 6 True ++ false


-- all operations on a stack of depth 5, of type Word16 and with
-- start/default value 0
stackStream :: Stream Word16
stackStream = stack (5::Int) 0 popSignal pushSignal pushValue


stackTest :: Spec
stackTest = do
  observer "stack" stackStream


stackExamples :: IO ()
stackExamples = do
  prettyPrint stackTest
  putStrLn ""
  putStrLn ""
  interpret 15 stackTest
