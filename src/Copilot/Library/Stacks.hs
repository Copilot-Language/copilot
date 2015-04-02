-- | 
-- Module: Stacks
-- Description: Stream for a stack machine
-- Copyright: (c) 2011 National Institute of Aerospace / Galois, Inc.
-- 
-- This is a stream for a stack machine.
--
-- The stack is created from a specified depth, a specified start value, and
-- three input streams:
--
-- * a pop signal which pops off the stack when true,
-- * a push signal which pushes the value from the push stream onto the stack when true,
-- * a push stream.
-- The resultant stream is the top value of the stack.
--
-- In 'stack' the push signal takes priority over the pop signal in the event
-- that both are true in the same tick. This priority is reversed in 'stack''.
-- 
-- Here is an example sequence with one stack of each type, both depth 3 and
-- starting value 0:
--
-- @ 
-- popSignal:   pushSignal:  pushValue:   stack:       stack':     
-- false        true         100          0            0           
-- false        true         101          100          100         
-- true         true         102          101          101         
-- true         false        103          100          102         
-- true         false        104          0            101         
-- true         false        105          0            100         
-- true         false        106          0            0           
-- @
-- 
-- Note the difference at the 4th tick after /popSignal/ and /pushSignal/ were
-- both true.  Note also that one cannot pop the start value off the stack -
-- that is, the stack is never empty.

{-# LANGUAGE NoImplicitPrelude #-}

module Copilot.Library.Stacks
  ( stack, stack' ) where

import Copilot.Language

-- | Stack stream in which the pop signal has precedence over the push signal
-- in case both are true in the same tick
stack :: (Integral a, Typed b) =>
         a              -- ^ Depth
         -> b           -- ^ Start value
         -> Stream Bool -- ^ Pop signal
         -> Stream Bool -- ^ Push signal
         -> Stream b    -- ^ Push stream
         -> Stream b    -- ^ Stack top
stack depth startValue
  popSignal pushSignal pushValue =
  let depth'      = fromIntegral depth
      startValue' = constant startValue
      stackValue pushValue' popValue' =
        let stackValue'  = [ startValue ]
                           ++ mux popSignal
                                  popValue'
                                  ( mux pushSignal
                                        pushValue'
                                        stackValue' )
        in  stackValue'
      toStack l =
        let toStack' _    []           = startValue'
            toStack' prev ( sv : svs ) =
              let current = sv prev ( toStack' current svs )
              in  current
        in toStack' pushValue l

   in toStack $ replicate depth' stackValue

-- | Stack stream in which the push signal has precedence over the pop signal
-- in case both are true in the same tick
stack' :: (Integral a, Typed b) =>
         a              -- ^ Depth
         -> b           -- ^ Start value
         -> Stream Bool -- ^ Pop signal
         -> Stream Bool -- ^ Push signal
         -> Stream b    -- ^ Push stream
         -> Stream b    -- ^ Stack top
stack' depth startValue
  popSignal pushSignal pushValue =
  let depth'      = fromIntegral depth
      startValue' = constant startValue
      stackValue pushValue' popValue' =
        let stackValue'  = [ startValue ]
                           ++ mux pushSignal
                                  pushValue'
                                  ( mux popSignal
                                        popValue'
                                        stackValue' )
        in  stackValue'
      toStack l =
        let toStack' _    []           = startValue'
            toStack' prev ( sv : svs ) =
              let current = sv prev ( toStack' current svs )
              in  current
        in toStack' pushValue l

   in toStack $ replicate depth' stackValue
