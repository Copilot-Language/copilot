module Copilot.Examples.LTLExamples where


import qualified Prelude as P
import Copilot.Language
import Copilot.Language.Prelude hiding ( until )
import Copilot.Library.LTL


----------------
-- LTL tests ---
----------------


testAlways :: Int -> Int -> Stream Bool
testAlways i1 i2 = let input = replicate i1 P.True P.++ [ False ] ++ true
                   in  always i2 input


testNext :: Int -> Stream Bool
testNext i1 = let input = [ False, False, True ] ++ input
              in  next input


testFuture :: Int -> Int -> Stream Bool
testFuture i1 i2 = let input = replicate i1 False P.++ [ True ] ++ false
                   in  eventually i2 input


testUntil :: Int -> Int -> Int -> Stream Bool
testUntil i1 i2 i3 =
    let t0 = replicate i1 True  ++ false
        t1 = replicate ( i2 - 1 ) False P.++ [ True ] ++ false
    in until i3 t0 t1


testRelease :: Int -> Int -> Int -> Stream Bool
testRelease i1 i2 i3 =
    let t0 = replicate i1 True ++ false
        t1 = replicate ( i2 - 1 ) False P.++ [ True ] ++ false
    in release i3 t1 t0


ltlTest :: Spec
ltlTest = do
  trigger "testAlways1" true [ arg $ testAlways  0  0    ]
  trigger "testAlways2" true [ arg $ testAlways  5  1    ]
  trigger "testNext"    true [ arg $ testNext    10      ]
  trigger "testFuture"  true [ arg $ testFuture  9  10   ]
  trigger "testUntil"   true [ arg $ testUntil   5  6  4 ]
  trigger "testRelease" true [ arg $ testRelease 5  5  4 ]


test = do
  prettyPrint ltlTest
  putStrLn ""
  putStrLn ""
  interpret 20 [] ltlTest
