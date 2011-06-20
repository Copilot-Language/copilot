-- | Basic bounded statistics.  In the following, a bound @n@ is given stating
-- the number of periods over which to compute the statistic (@n == 1@ computes
-- it only over the current period).

module Copilot.Library.Statistics
    ( max, min, sum, mean, meanNow ) where


import Copilot.Language
import qualified Prelude as P
import Copilot.Language.Prelude hiding ( sum, max, min )
import Copilot.Library.Utils

-- | Summation.
sum :: ( Typed a, Num a ) => Int -> Stream a -> Stream a
sum n s = nfoldl1 n (+) s

-- | Maximum value.
max :: ( Typed a, Ord a ) => Int -> Stream a -> Stream a
max n s = nfoldl1 n largest s
    where largest  = \ x y -> mux ( x >= y ) x y

-- | Minimum value.
min :: ( Typed a, Ord a ) => Int -> Stream a -> Stream a
min n s = nfoldl1 n smallest s
    where smallest = \ x y -> mux ( x <= y ) x y

-- | Mean value.  @n@ must not overflow
-- for word size @a@ for streams over which computation is peformed.
mean :: ( Typed a, Fractional a ) => Int -> Stream a -> Stream a
mean n s = ( sum n s ) / ( fromIntegral n )

-- | Mean value over the current set of specs passed in.
meanNow :: (Typed a, Integral a) => [Stream a] -> Stream a
meanNow [] = error
    "Error in majority: list of arguments must be nonempty."
meanNow ls = ( foldl1 (+) ls ) `div` ( fromIntegral $ length ls )
