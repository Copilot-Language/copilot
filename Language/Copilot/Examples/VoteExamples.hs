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

-- | Computes an alleged majority over streams and determines if the computed
-- value is the majority.
maj1 :: Streams
maj1 = do
  let v0  = varW32 "v0"
      v1  = varW32 "v1"
      v2  = varW32 "v2"
      -- v3  = varW32 "v3"
      -- v4  = varW32 "v4"

      -- v5  = varW32 "v5"
      -- v6  = varW32 "v6"
      -- v7  = varW32 "v7"
      -- v8  = varW32 "v8"
      -- v9  = varW32 "v9"
      -- v10  = varW32 "v10"
      -- v11 = varW32 "v11"
      -- v12  = varW32 "v12"

      ans = varW32 "ans"
--      chk = varB "chk"
      ls = [v0, v1, v2] --, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12]
  v0  .= [3] ++ v0 + 1
  v1  .= [3] ++ v1 + 1
  v2  .= [1] ++ v2 + 1
  -- v3  .= [3] ++ v3 + 1
  -- v4  .= [1] ++ v4 + 1
  -- v5  .= [3] ++ v5 + 1
  -- v6  .= [3] ++ v6 + 1
  -- v7  .= [1] ++ v7 + 1
  -- v8  .= [3] ++ v8 + 1
  -- v9  .= [1] ++ v9 + 1
  -- v10  .= [3] ++ v10 + 1
  -- v11  .= [3] ++ v11 + 1
  -- v12  .= [1] ++ v12 + 1

  ans .= majority ls
--  chk .= aMajority ls ans

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
  let v2 = varW32 "v2"
      v3 = varW32 "v3"
      ls = [8, 3, 7, 6, 5, 4, 2, 4, 1, 0]
  v3 .= ftAvg ls 3
  v2 .= ftAvg ls 2


-------------------------------------------------------------------
-- distributed example
-------------------------------------------------------------------
makeProcs :: IO ()
makeProcs = do
  compile proc0 "proc0" $ 
    setCode (Just (headers P.++ send0), Just mainStr) baseOpts
  compile proc1 "proc1" $ 
    setCode (Just (headers P.++ send1), Nothing) baseOpts
  compile proc2 "proc2" $ 
    setCode (Just (headers P.++ send2), Nothing) baseOpts
  exitCode <- system "gcc -o proc -Wall proc0.c proc1.c proc2.c"
  print exitCode
  where headers = includes P.++ vars P.++ decls

body :: Int -> Spec Word16 -> [Spec Word16] -> Spec Word16 -> Streams
body id p ls exp = do
  let maj = varW16 "maj"
      chk = varB "chk"
      pd  = varW16 "pd"
  p   .= exp
  pd  .= drop 1 p -- because we sample last round's vars
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
  body 0 p0 [p1, p2] ([0,1,2] ++ p0)

proc1 = do
  let p1 = varW16 "p1"
      p0 = extW16 "p0"
      p2 = extW16 "p2"
  body 1 p1 [p0, p2] ([1,0,2,0] ++ p1)

proc2 = do
  let p2 = varW16 "p2"
      p0 = extW16 "p0"
      p1 = extW16 "p1"
  body 2 p2 [p0, p1] ([2,1,0,2,1] ++ p1)

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
