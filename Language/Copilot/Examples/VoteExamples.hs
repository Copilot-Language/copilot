module Language.Copilot.Examples.VoteExamples where

import Language.Copilot
import Language.Copilot.AdHocC
--import Language.Copilot.Core
import Language.Copilot.Libs.Vote
import Data.Word
import Data.Int
import System.Cmd (system)
import System.Exit (ExitCode(..))

import Prelude (IO(..), ($), Int, String, unlines, Maybe(..), putStrLn)
import qualified Prelude as P

-- | Computes an alleged majority over constants and determines if 3 is the
-- majority.
maj0 :: Streams
maj0 = do
  let v = varW32 "v"
      b = varB "b"
      ls = [3, 2, 3, 0, 3, 3, 0]
  v .= majority ls
  b .= aMajority ls 3

-- | Computes an alleged majority over streams and determines if the computed
-- value is the majority.
maj1 :: Streams
maj1 = do
  let v0  = varW32 "v0"
      v1  = varW32 "v1"
      v2  = varW32 "v2"
      v3  = varW32 "v3"
      v4  = varW32 "v4"
      ans = varW32 "ans"
      chk = varB "chk"
      ls = [v0, v1, v2, v3, v4]
  v0  .= [3] ++ v0 + 1
  v1  .= [3] ++ v1 + 1
  v2  .= [1] ++ v2 + 1
  v3  .= [3] ++ v3 + 1
  v4  .= [1] ++ v4 + 1
  ans .= majority ls
  chk .= aMajority ls ans

maj2 :: Streams
maj2 = do
  let v = varB "v"
      b = varB "b"
      ls = [true, false, true, false, false, false, true]
  v .= majority ls
  b .= aMajority ls v

ft0 :: Streams
ft0 = do
  let v2 = varW32 "v2"
      v3 = varW32 "v3"
      ls = [8, 3, 7, 6, 5, 4, 2, 4, 1, 0]
  v3 .= ftAvg ls 3
  v2 .= ftAvg ls 2

-------------------------------------------------------------------
makeProcs :: IO ()
makeProcs = do
  compile proc0 "proc0" $ 
    setCode (Just (includes P.++ vars P.++ decls P.++ send0), Just mainStr) baseOpts
  compile proc1 "proc1" $ 
    setCode (Just (includes P.++ vars P.++ decls P.++ send1), Nothing) baseOpts
  compile proc2 "proc2" $ 
    setCode (Just (includes P.++ vars P.++ decls P.++ send2), Nothing) baseOpts
  exitCode <- system "gcc -o proc -Wall proc0.c proc1.c proc2.c"
  putStrLn (P.show exitCode)

body :: Int -> Spec Word16 -> [Spec Word16] -> Spec Word16 -> Streams
body id p ls exp = do
  let maj = varW16 "maj"
      chk = varB "chk"
      pd  = varW16 "pd"
  p   .= exp
  pd  .= drop 1 p -- because we sample las round's vars
  maj .= majority (pd:ls)
  chk .= aMajority ls maj
  send ("send" P.++ P.show id) (port 1) p
  send ("send" P.++ P.show id) (port 2) p

-- | Distributed majority voting among three processors.
proc0, proc1, proc2 :: Streams
proc0 = do
  let p0 = varW16 "p0"
      p1 = extW16 "p1"
      p2 = extW16 "p2"
      ls = [p1, p2]
  body 0 p0 ls ([0,1] ++ (p0 + 1) `mod` 3)

proc1 = do
  let p1 = varW16 "p1"
      p0 = extW16 "p0"
      p2 = extW16 "p2"
      ls = [p0, p2]
  body 1 p1 ls ([1,0] ++ (p1 + 2) `mod` 3)

proc2 = do
  let p2 = varW16 "p2"
      p0 = extW16 "p0"
      p1 = extW16 "p1"
      ls = [p0, p1]
  body 2 p2 ls ([2,0] ++ (p2 + 1) `mod` 3)



mainStr :: String 
mainStr = unlines 
  [ "int main (void) {"
  , "  int rnds;"
  , "  for(rnds = 0; rnds < 20; rnds++) {"
  , "    proc0();"
  , "    proc1();"
  , "    proc2();"
  , "    if (rnds % 5 == 0) {"
  , "      printf(\"proc0 maj: %u  \", copilotStateproc0.proc0.maj);"
  , "      printf(\"chk: %u\\n\", copilotStateproc0.proc0.chk);"
  , "    }"
  , "  }"
  , "  return 0;"
  , "}"
  ]

vars :: String
vars = unlines 
  [ "uint16_t p0;"
  , "uint16_t p1;"
  , "uint16_t p2;"
  ]

decls :: String
decls = unlines
  [ funcDecl Nothing "proc0" []
  , funcDecl Nothing "proc1" []
  , funcDecl Nothing "proc2" []
  ]

includes :: String
includes = unlines
  [ includeBracket "stdlib.h"
  , includeBracket "stdio.h"]

sendBody :: Int -> String
sendBody id = unlines
  [ "void send" P.++ P.show id P.++ "(uint16_t val, int port) {"
  , "  switch (port) {"
  , "  case 1: p" P.++ (P.show $ (id + 1) `P.mod` 3) P.++ " = val;"
  , "  case 2: p" P.++ (P.show $ (id + 2) `P.mod` 3) P.++ " = val;"
  , "  }"
  , "}"
  ]

send0, send1, send2 :: String
send0 = sendBody 0
send1 = sendBody 1
send2 = sendBody 2
