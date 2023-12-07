import Prelude hiding ( mod, (&&), (++), (/=), (<), (==), (>) )

import Copilot.Compile.C99
import Language.Copilot    hiding ( not )

import CopilotData.Data.LTL
import CopilotData.Data.LTL3

main :: IO ()
main = do
  interpret 10 spec
  spec' <- reify spec
  compile "c99" spec'

spec :: Spec
spec = do
  trigger "values" true [arg $ monitor [(55, values)] (ltlGlobally (LTLProp 55))]

values :: Stream Bool
values = extern "values" (Just [ True, True, False, True, False, True, True, False, True, False, True ])
