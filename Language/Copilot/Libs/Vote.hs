-- | Voting algorithms over streams of data.

module Language.Copilot.Libs.Vote 
    (majority, aMajority, ftAvg) where

import Prelude 
  ( ($), fromIntegral, error, length, otherwise
  , Bounded(..), maxBound, minBound, Int, map)
import Data.List (replicate, foldl')
import qualified Prelude as P 
import Data.Word

import Language.Copilot.Language
import Language.Copilot.Core
import qualified Language.Atom as A

-- | Boyer-Moore linear majority algorithm.  Warning!  Returns an arbitary
-- element if no majority is returned.  See 'aMajority' below.
majority :: (Streamable a, A.EqE a) => [Spec a] -> Spec a
majority [] = 
  error "Error in majority: list of arguments must be nonempty."
majority ls = majority' ls (const unit) 0

majority' :: (Streamable a, A.EqE a) 
          => [Spec a] -> Spec a -> Spec Word32 -> Spec a
majority' [] candidate _ = candidate
majority' (x:xs) candidate cnt = 
  majority' xs (mux (cnt == 0) x candidate)
               (mux (cnt == 0 || x == candidate) (cnt + 1) (cnt - 1))

-- | Is the candidate majority value the actual majority value?
aMajority :: (Streamable a, A.EqE a) => [Spec a] -> Spec a -> Spec Bool
aMajority [] _ = 
  error "Error in aMajority: list of arguments must be nonempty."
aMajority ls candidate = 
  let m = map (\v -> mux (v == candidate) 1 (0 :: Spec Word32)) ls in
  (foldl' (+) 0 m) * 2 > (fromIntegral $ length ls)
  
-- | Fault-tolerant average.  Throw away the bottom and top @n@ elements and
-- take the average of the rest.  Return an error if there are less than @2 * n
-- + 1@ elements in the list.
ftAvg :: (Streamable a, A.IntegralE a, Bounded a) => [Spec a] -> Int -> Spec a
ftAvg ls n | length ls P.<= 2 P.* n = 
  error $      "Error in ftAvg: list of arguments must be at least " 
          P.++ P.show (2 * n + 1) P.++ "."
           | otherwise =       (ftAvg' ls (replicate n low) (replicate n high) 0)
                         `div` (fromIntegral $ length ls P.- (2 P.* n))
  where low = const maxBound
        high = const minBound

-- | Return the total sum of values, minus the high and low values.
ftAvg' :: (Streamable a, A.IntegralE a, Bounded a) 
       => [Spec a] -> [Spec a] -> [Spec a] -> Spec a -> Spec a
ftAvg' [] lows highs sum = foldl' (-) sum (lows P.++ highs)
ftAvg' (x:xs) lows highs sum = 
  ftAvg' xs (insert (<) lows x) (insert (>) highs x) (sum + x)

-- | Insert an element into an ordered list.
-- insert :: (Streamable a, A.OrdE a) 
--        => (Spec a -> Spec a -> Spec Bool) 
--        -> [Spec a] -> Spec a -> Int -> [Spec a]
-- insert _ xs _ idx   | idx P.== length xs = []
-- insert ord xs x idx | otherwise =
--   let n = xs !! idx
--       choice = mux (x `ord` n) x n in
--   choice : insert ord xs choice (idx + 1)
insert :: (Streamable a, A.OrdE a) 
       => (Spec a -> Spec a -> Spec Bool) 
       -> [Spec a] -> Spec a -> [Spec a]
insert _ [] _ = []
insert ord (n:ns) x = 
  let pred = x `ord` n in
  mux pred x n : insert ord ns (mux pred n x) 
