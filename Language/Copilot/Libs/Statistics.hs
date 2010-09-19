-- | Basic bounded statistics.  In the following, a bound @n@ is given stating
-- the number of periods over which to compute the statistic (@n == 1@ computes
-- it only over the current period). 

module Language.Copilot.Libs.Statistics(max, min, sum, mean) where

import Prelude (Int, ($), foldl1, fromIntegral)
import qualified Prelude as P 

import qualified Language.Atom as A

import Language.Copilot.Libs.ErrorChks (nOneChk)
import Language.Copilot.Core
import Language.Copilot.Language

foldDrops :: (Streamable a) => Int -> (Spec a -> Spec a -> Spec a) -> Spec a -> Spec a
foldDrops n f s = foldl1 f [drop x s | x <- [0..(n-1)]]

-- | Summation.
sum :: (Streamable a, A.NumE a) => Int -> Spec a -> Spec a
sum n s = 
  nOneChk "sum" n $ foldDrops n (+) s 

-- | Maximum value.
max :: (Streamable a, A.NumE a) => Int -> Spec a -> Spec a
max n s = 
  nOneChk "max" n $ foldDrops n largest s 
  where largest = \ x y -> mux (x <= y) y x

-- | Minimum value.
min :: (Streamable a, A.NumE a) => Int -> Spec a -> Spec a
min n s = 
  nOneChk "max" n $ foldDrops n smallest s
  where smallest = \ x y -> mux (x <= y) x y

-- | Mean value.  @n@ must not overflow
-- for word size @a@ for streams over which computation is peformed.
mean :: (Streamable a, Fractional a, A.NumE a) => Int -> Spec a -> Spec a
mean n s = 
  nOneChk "mean" n $ (sum n s) / (const $ fromIntegral n)

-- majority :: (Streamable a, A.NumE a) => Int -> Spec a -> Spec a
-- majority n s =
