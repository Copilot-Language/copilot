module Language.Copilot.Examples.RegExpExample where

import Language.Copilot.Core
import qualified Language.Copilot.Language as C
import Language.Copilot.Interface
import Language.Copilot.Libs.RegExp ( copilotRegexp )

testRegExp :: Streams
testRegExp = do 
  let input  = C.varI64 "input"
      output = C.varB   "output"
      reset  = C.varB   "reset"
  input C..= [ 0, 1, 2 ] C.++ (-3)
  reset C..= [ True ] C.++ C.false
  copilotRegexp input "<0><1><2>*|<-3>+" output reset

main :: IO ()
main = do
  interpret testRegExp 10 baseOpts
  compile testRegExp "testRegExp" baseOpts
