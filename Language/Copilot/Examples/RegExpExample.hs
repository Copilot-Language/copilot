module Main where

import Language.Copilot.Core
import qualified Language.Copilot.Language as C
import Language.Copilot.Interface
import Language.Copilot.Libs.RegExp ( copilotRegexp )


testRegExp = do { let input  = C.varI64 "input"
                      output = C.varB   "output"
                      reset  = C.varB   "reset"
                ; input C..= [ 0, 1, 2 ] C.++ Const (-3)
                ; reset C..= [ True ] C.++ Const False
                ; copilotRegexp input "<0><1><2>*|<-3>+" output reset
                }

main = do
  interpret testRegExp 10 baseOpts
  compile testRegExp "testRegExp" baseOpts
  return ()
