module Language.Copilot.Examples.VoteExamples where

import Language.Copilot
import Language.Copilot.AdHocC
--import Language.Copilot.Core
import Language.Copilot.Libs.Vote
import Data.Word
import Data.Int
import System.Cmd (system)
import System.Exit (ExitCode(..))

import Prelude (IO(..), ($), Int, String, unlines, Maybe(..), print)
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

main = interpret maj1 20 baseOpts

-- | Computes an alleged majority over streams and determines if the computed
-- value is the majority.
maj1 :: Streams
maj1 = do
  let v0  = varW32 "v0"
      v1  = varW32 "v1"
      v2  = varW32 "v2"
      v3  = varW32 "v3"
      v4  = varW32 "v4"
      v5  = varW32 "v5"
      v6  = varW32 "v6"
      v7  = varW32 "v7"
      v8  = varW32 "v8"
      ans = varW32 "ans"
      chk = varB "chk"
      ls = [v0, v1, v2, v3, v4, v5, v6] --, v7, v8] --, v9, v10, v11, v12]
  v0  .= [3,7] ++ v0 
  v1  .= [7] ++ v1 
  v2  .= [1] ++ v2 + 1
  v3  .= [3] ++ v3 + 1
  v4  .= [1] ++ v4 + 1
  v5  .= [3] ++ v5 + 1
  v6  .= [3] ++ v6 + 1
  v7  .= [3] ++ v7 + 1
  v8  .= [3] ++ v8 + 1
  ans .= majority ls
  chk .= aMajority ls ans

maj2 :: Streams
maj2 = do
  let v = varB "v"
      b = varB "b"
      ls = [true, false, true, false, false, false, true]
  v .= majority ls
  b .= aMajority ls v

maj3 :: Streams
maj3 = do
  let x    = extW32 "x"
      y    = extW32 "y"
      z    = extW32 "z"
      ans  = varW32 "ans"
      chk  = varB "chk"
      exts = [x, y, z]
  ans .= majority exts
  chk .= aMajority exts ans
  

ft0 :: Streams
ft0 = do
  let v3 = varW32 "v3"
      ls = [8, 3, 7, 4, 2, 4, 1, 0]
  v3 .= ftAvg ls 3

-- Use CBMC to find the overflow
ftAvgOverflow :: Streams
ftAvgOverflow = do
  let v = varW16 "v"
  v .= ftAvg [40000, 40000, 40000, 40000] 1

-------------------------------------------------------------------
-- distributed example
-------------------------------------------------------------------
makeProcs :: IO ()
makeProcs = do
  distCompile proc0 "proc0" send0 (Just mainStr)
  distCompile proc1 "proc1" send1 Nothing
  distCompile proc2 "proc2" send2 Nothing
  exitCode <- system "gcc -o proc -Wall proc0.c proc1.c proc2.c"
  print exitCode

distCompile :: Streams -> String -> String -> Maybe String -> IO ()
distCompile streams fileName addHeaders mMain =
  compile streams fileName $ 
    setCode ( Just (headers P.++ addHeaders)
            , mMain) 
            baseOpts
  where headers = includes P.++ vars P.++ decls
  

-- | Distributed majority voting among three processors.
proc0, proc1, proc2 :: Streams
proc0 = do
  let p0  = varW16 "p0"
      p01 = varW16 "p01"
      p02 = varW16 "p02"
      p1  = extW16 "p1"
      p2  = extW16 "p2"
      maj = varW16 "maj"
      chk = varB "chk"
      ls  = [p0, p01, p02]
  p0  .= [3,4] ++ p0
  p01 .= p1
  p02 .= p2
  maj .= majority ls 
  chk .= aMajority ls maj
  trigger true "send0" p0

proc1 = do
  let p1 = varW16 "p1"
      p0 = varW16 "p10"
      p2 = varW16 "p12"
  p1  .= [7,8] ++ p1
  trigger true "send1" p1

proc2 = do
  let p2 = varW16 "p2"
      p0 = varW16 "p20"
      p1 = varW16 "p21"
  p2  .= [7,8] ++ p2
  trigger true "send2" p2

sendBody :: Int -> String
sendBody id = unlines
  [ "void send" P.++ P.show id P.++ "(uint16_t val) {"
  , "  p" P.++ P.show id P.++ " = val;"
  , "}"
  ]

mainStr :: String 
mainStr = unlines 
  [ "int main (void) {"
  , "  int rnds;"
  , "  for(rnds = 0; rnds < 10; rnds++) {"
  , "    __proc0();"
  , "    __proc1();"
  , "    __proc2();"
  , "      printf(\"p0: %u, p1: %u, p2: %u\\n\", copilotStateproc0.proc0.p0, copilotStateproc0.proc0.p01, copilotStateproc0.proc0.p02);"
  , "      printf(\"   proc0 maj: %u  \", copilotStateproc0.proc0.maj);"
  , "      printf(\"chk: %u\\n\", copilotStateproc0.proc0.chk);"
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
  [ funcDecl Nothing "__proc0" []
  , funcDecl Nothing "__proc1" []
  , funcDecl Nothing "__proc2" []
  ]

includes :: String
includes = unlines
  [ includeBracket "stdlib.h"
  , includeBracket "stdio.h"]

send0, send1, send2 :: String
send0 = sendBody 0
send1 = sendBody 1
send2 = sendBody 2
