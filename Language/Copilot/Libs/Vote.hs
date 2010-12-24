-- | Voting algorithms over streams of data.

module Language.Copilot.Libs.Vote 
    (majority, aMajority) where

import Prelude (($), fromIntegral, error, length)
import Data.List (foldl')
import qualified Prelude as P 
import Data.Word
import Data.Int

import Language.Copilot.Libs.Statistics (meanNow)
import Language.Copilot.Language
import Language.Copilot.Core
import qualified Language.Atom as A

-- | Boyer-Moore linear majority algorithm.  Warning!  Returns an arbitary
-- element if no majority is returned.  See 'aMajority' below.
majority :: (Streamable a, A.EqE a) => [Spec a] -> Spec a
majority [] = 
  error "Error in majority: list of arguments must be nonempty."
majority ls = majority' ls 0 0

majority' :: (Streamable a, A.EqE a) => [Spec a] -> Spec a -> Spec Word32 -> Spec a
majority' [] candidate cnt = candidate
majority' (x:xs) candidate cnt = 
  mux (cnt == 0) 
      (majority' xs x (cnt + 1))
      (mux (x == candidate) 
           (majority' xs candidate (cnt + 1))
           (majority' xs candidate (cnt - 1)))

aMajority :: (Streamable a, A.EqE a) => [Spec a] -> Spec a -> Spec Bool
aMajority [] candidate = 
  error "Error in aMajority: list of arguments must be nonempty."
aMajority ls candidate = 
  (foldl' (\cnt x -> mux (x == candidate)
                        (cnt + 1)
                        cnt :: Spec Word32
          ) 0 ls) * 2
  > (fromIntegral $ length ls)

-- | Fault-tolerant average.  Throw away the bottom and top third and take the
-- average of the rest.
ftAvg :: (Streamable a, A.EqE a) => [Spec a] -> Spec a
ftAvg [] = 
  error "Error in aMajority: list of arguments must be nonempty."
ftAvg ls = ftAvg' ls 0 0

