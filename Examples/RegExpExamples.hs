-- | Regular expression library tests for the
-- copilotRegexp and copilotRegexpB functions

module Copilot.Examples.RegExpExamples where


import qualified Prelude as P
import Copilot.Language
import Copilot.Library.RegExp
import Data.Bool


reset :: Stream Bool
reset = false


-- | Regular expression matching on integral streams

s :: Stream Int8
s = [ 0, 1, 2 ] ++ constant (-3)

test1 = copilotRegexp s "<0><1><2>*|<-3>+" reset


-- | Regular expressions over boolean streams

s0    = [ True,  False, False, False, False, False ] ++ s1
s1    = [ False, True,  False, False, False, False ] ++ s2
s2    = [ False, False, True,  False, False, False ] ++ s3
s3    = [ False, False, False, True,  False, False ] ++ s4
s4    = [ False, False, False, False, True,  False ] ++ s5
s5    = [ False, False, False, False, False, True  ] ++ s0


test2 = copilotRegexpB
       "<s0><s1><s2><s3><s4><s5>(<s0>|<s1>|<s2>|<s3>|<s4>|<s5>)+"
       [ ( "s0", s0 )
       , ( "s1", s1 )
       , ( "s2", s2 )
       , ( "s3", s3 )
       , ( "s4", s4 )
       , ( "s5", s5 ) ] false


spec = do
   observer "test1" test1
   observer "test2" test2


test = do
   interpret 20 [] spec
