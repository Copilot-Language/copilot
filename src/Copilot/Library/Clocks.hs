-- | A library that generates new clocks based on a base period.
-- Usage, supposing @v@ is a Copilot variable, then
-- @
-- clk ( period 3 ) ( phase 1 )
-- @
-- is equivalent to a stream of values like:
-- @
-- cycle [False, True, False]
-- @
-- that generates a stream of values
-- @
-- False True False False True False False True False ...
-- 0     1    2     3     4    5     6     7    8
-- @
-- That is true every 3 ticks (the period) starting on the 1st tick (the phase).
-- Constraints:
-- The period must be greater than 0.
-- The phase must be greater than or equal to 0.
-- The phase must be less than the period.


module Copilot.Library.Clocks
  ( clk, clk1, period, phase ) where



import Prelude ( Integral, fromIntegral, ($), error )
import qualified Prelude as P
import Copilot.Language
import Data.Bool
import Data.List (replicate)


data ( Integral a ) => Period a = Period a
data ( Integral a ) => Phase  a = Phase  a


period :: ( Integral a ) => a -> Period a
period = Period

phase :: ( Integral a ) => a -> Phase a
phase  = Phase


-- clk generates a clock that counts n ticks by using an array of size n
clk :: ( Integral a ) => Period a -> Phase a -> Stream Bool
clk ( Period period' ) ( Phase phase' ) = clk'
  where clk' = if period' P.< 1 then
                   error ( "clock period must be 1 or greater." )
               else if phase' P.< 0 then
                        error ( "clock phase must be 0 or greater." )
                    else if phase' P.>= period' then
                             error ( "clock phase must be less than period.")
                         else replicate ( fromIntegral phase' ) False
                              P.++ True : replicate
                                   ( fromIntegral
                                     $ period' P.- phase' P.- 1 ) False
                                   ++ clk'


-- clk1 generates a clock that counts n ticks by using a
-- counter variable of integral type a
clk1 :: ( Integral a, Typed a )
        => Period a -> Phase a -> Stream Bool
clk1 ( Period period' ) ( Phase phase' ) =
    if period' P.< 1 then
        error ( "clock period must be 1 or greater." )
    else if phase' P.< 0 then
             error ( "clock phase must be 0 or greater." )
         else if phase' P.>= period' then
                  error ( "clock phase must be less than period.")
              else
                  let counter = [ P.fromInteger 0 ]
                                ++ mux ( counter /= ( constant $ period' P.- 1 ) )
                                       ( counter P.+ 1 )
                                       ( 0 )
                  in counter == fromIntegral phase'
