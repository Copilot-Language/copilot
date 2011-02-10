-- | A library that generates new clocks based on a base period.
-- Usage, supposing @v@ is a Copilot variable, then
-- @
-- v `clock` (period 3, phase 1)
-- @
-- is equivalent to
-- @
-- v .= [False, True, False] ++ v
-- @
-- generating a stream of values
-- @
-- False True False False True False False True False ...
-- 0     1    2     3     4    5     6     7    8
-- @
-- That is true every 3 ticks (the period) starting on the 1st tick (the phase).
-- Constraints:
-- The period must be greater than 0.
-- The phase must be greater than or equal to 0.
-- The phase must be less than the period.

module Language.Copilot.Libs.Clocks
  ( clock, period
  ) where

import Prelude (Int, error, String, show, (.), fromIntegral)
import qualified Prelude as P
import Data.List (replicate)
import Data.Int

import Language.Copilot.Language
import Language.Copilot.Core hiding (Period)

-- For testing.
import Language.Copilot.Interface (interpret, baseOpts)

data Period = Period Int
data Phase = Phase Int

period :: Int -> Period
period = Period

phase :: Int -> Phase
phase = Phase

clock :: Spec Bool -> (Period, Phase) -> Streams
clock v (Period per, Phase ph) =
  if (per P.< 1) then error ("Error in stream " P.++ (show v)
                             P.++ ": period must be 1 or greater.")
    else if (ph P.< 0) then error ("Error in stream " P.++ (show v)
                                   P.++ ": phase must be 0 or greater.")
      else if (ph P.>= per) then error ("Error in stream " P.++ (show v)
                                        P.++ ": phase must be less than period.")
             else v .= ((replicate ph False)
                          P.++ (True : (replicate ((per - ph) - 1) False)) ++ v)

clock' :: Spec Bool -> (Period, Phase) -> Streams
clock' v (Period per, Phase ph) = do
  { let counter = varI32 "counter"
  ; counter .= [ 0 ] ++ ( mux ( counter /= ( Const . fromIntegral ) per - 1 )
                              ( counter + 1 )
                              ( 0 ) )
  ; if (per P.< 1) then error ("Error in stream " P.++ (show v)
                               P.++ ": period must be 1 or greater.")
    else if (ph P.< 0) then error ("Error in stream " P.++ (show v)
                                   P.++ ": phase must be 0 or greater.")
         else if (ph P.>= per) then error ("Error in stream " P.++ (show v)
                                           P.++ ": phase must be less than period.")
              else v .= counter == ( Const . fromIntegral ) ph
  }

clkTest :: Streams
clkTest = do
  let x = varB "x"
  let y = varB "y"
  let z = varB "z"
  x `clock`  (period 3, phase 1)
  y `clock`  (period 4, phase 3)
  z `clock'` (period 4, phase 3)

test = interpret clkTest 100 baseOpts
