module Language.Copilot.Examples.RegExpExample where

import Language.Copilot.Core
import qualified Language.Copilot.Language as C
import Language.Copilot.Interface
import Language.Copilot.Libs.RegExp ( copilotRegexp, copilotRegexpB )

testRegExp1 :: Streams
testRegExp1 = do
  let input  = C.varI64 "input"
      output = C.varB   "output"
      reset  = C.varB   "reset"
  input C..= [ 0, 1, 2 ] C.++ (-3)
  reset C..= [ True ] C.++ C.false
  copilotRegexp input "<0><1><2>*|<-3>+" output reset

testRegExp2 :: Streams
testRegExp2 = do
  let b1     = C.varB "b1"
      b2     = C.varB "b2"
      b3     = C.varB "b3"
      output = C.varB "output"
      reset  = C.varB "reset"
  b1 C..= [ True,  False, False ] C.++ b2
  b2 C..= [ False, True,  False ] C.++ b3
  b3 C..= [ False, False,  True ] C.++ b1
  reset C..= [ True ] C.++ C.false
  copilotRegexpB "<b1><b2><b3>(<b1>|<b2>|<b3>)*" output reset

main :: IO ()
main = do
  interpret testRegExp1 10 baseOpts
  compile testRegExp1 "testRegExp" baseOpts

  interpret testRegExp2 10 baseOpts
  compile testRegExp2 "testRegExp" baseOpts
