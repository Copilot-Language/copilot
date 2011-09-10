{-# LANGUAGE NoImplicitPrelude #-}

module Copilot.Library.Stacks
  ( stack, stack' ) where


import Copilot.Language
import Copilot.Language.Prelude


type PushSignal    = Stream Bool
type PopSignal     = Stream Bool
type PushStream  a = Stream a
type StackTop    a = Stream a


-- stack and stack' streams
--
-- for the stack stream, the PopSignal has precedence
-- over the PushSignal in case both are true in the same tick
--
-- for the stack' stream, the PushSignal has precedence
-- over the PopSignal in case both are true in the same tick
stack, stack' :: ( Integral a, Typed b )
         => a -> b
         -> PopSignal
         -> PushSignal
         -> PushStream b
         -> StackTop   b
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
