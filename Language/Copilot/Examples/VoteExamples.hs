module Language.Copilot.Examples.LTLExamples where

-- import Prelude (replicate, Int, replicate)
-- import qualified Prelude as P

import Language.Copilot
import Language.Copilot.Libs.Vote

import Prelude (IO(..), ($))

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
  compile proc0 "proc0" $ setPP ("","") baseOpts
  compile proc1 "proc1" $ setPP ("","") baseOpts
  compile proc2 "proc2" $ setPP ("","") baseOpts

-- | Distributed majority voting among three processors.
proc0, proc1, proc2 :: Streams
proc0 = do
  let p0 = varW16 "p0"
      p01 = extW16 "p01"
      p02 = extW16 "p02"
      maj = varW16 "maj"
      chk = varB "chk"
      ls = [p0, p01, p02]
  p0  .= [0] ++ p0 + 1
  maj .= majority ls
  chk .= aMajority ls maj
  send "send0" (port 1) p0
  send "send0" (port 2) p0

proc1 = do
  let p1 = varW16 "p1"
      p10 = extW16 "p10"
      p12 = extW16 "p12"
      maj = varW16 "maj"
      chk = varB "chk"
      ls = [p10, p1, p12]
  p1  .= [0] ++ p1 + 1
  maj .= majority ls
  chk .= aMajority ls maj
  send "send1" (port 0) p1
  send "send1" (port 2) p1

proc2 = do
  let p2 = varW16 "p2"
      p20 = extW16 "p20"
      p21 = extW16 "p21"
      maj = varW16 "maj"
      chk = varB "chk"
      ls = [p20, p21, p2]
  p2  .= [0] ++ p2 + 1
  maj .= majority ls
  chk .= aMajority ls maj
  send "send2" (port 0) p2
  send "send2" (port 1) p2

