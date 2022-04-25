-- |
-- Module: Clocks
-- Description: Clocks based on a base period and phase
-- Copyright: (c) 2011 National Institute of Aerospace / Galois, Inc.
--
-- This library generates new clocks based on a base period and phase.
--
-- = Example Usage
--
-- Also see @examples/Clock.hs@ in the
-- <https://github.com/Copilot-Language/copilot/blob/master/copilot/examples/ Copilot repository>.
--
-- @
--     'clk' ( 'period' 3 ) ( 'phase' 1 )
-- @
--
-- is equivalent to a stream of values like:
--
-- @
--     cycle [False, True, False]
-- @
--
-- that generates a stream of values
--
-- @
--     False True False False True False False True False ...
--     0     1    2     3     4    5     6     7    8
-- @
--
-- That is true every 3 ticks (the period) starting on the 1st tick (the phase).

{-# LANGUAGE NoImplicitPrelude #-}

module Copilot.Library.Clocks
  ( clk, clk1, period, phase ) where

import Prelude ()
import qualified Prelude as P
import Copilot.Language

data ( Integral a ) => Period a = Period a
data ( Integral a ) => Phase  a = Phase  a

-- | Constructor for a 'Period'. Note that period must be greater than 0.
period :: ( Integral a ) => a -> Period a
period = Period

-- | Constructor for a 'Phase'. Note that phase must be greater than or equal
-- to 0, and must be less than the period.
phase :: ( Integral a ) => a -> Phase a
phase  = Phase

-- | Generate a clock that counts every @n@ ticks, starting at tick @m@, by
-- using an array of size @n@.
clk :: ( Integral a ) =>
       Period a       -- ^ Period @n@ of clock
       -> Phase a     -- ^ Phase @m@ of clock
       -> Stream Bool -- ^ Clock signal - 'True' on clock ticks, 'False' otherwise
clk ( Period period' ) ( Phase phase' ) = clk'
  where clk' = if period' P.< 1 then
                   badUsage ( "clk: clock period must be 1 or greater" )
               else if phase' P.< 0 then
                        badUsage ( "clk: clock phase must be 0 or greater" )
                    else if phase' P.>= period' then
                             badUsage ( "clk: clock phase must be less than period")
                         else replicate ( fromIntegral phase' ) False
                              P.++ True : replicate
                                   ( fromIntegral
                                     $ period' P.- phase' P.- 1 ) False
                                   ++ clk'


-- | This follows the same convention as 'clk', but uses a counter variable of
-- integral type /a/ rather than an array.
clk1 :: ( Integral a, Typed a ) =>
        Period a       -- ^ Period @n@ of clock
        -> Phase a     -- ^ Phase @m@ of clock
        -> Stream Bool -- ^ Clock signal - 'True' on clock ticks, 'False' otherwise
clk1 ( Period period' ) ( Phase phase' ) =
    if period' P.< 1 then
        badUsage ( "clk1: clock period must be 1 or greater" )
    else if phase' P.< 0 then
             badUsage ( "clk1: clock phase must be 0 or greater" )
         else if phase' P.>= period' then
                  badUsage ( "clk1: clock phase must be less than period")
              else
                  let counter = [ P.fromInteger 0 ]
                                ++ mux ( counter /= ( constant $
                                                        period' P.- 1 ) )
                                       ( counter P.+ 1 )
                                       ( 0 )
                  in counter == fromIntegral phase'
