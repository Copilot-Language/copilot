{-# LANGUAGE ScopedTypeVariables #-}
-- | Simulate state machines using streams.
module Copilot.Library.StateMachines where

import Copilot.Language (Stream, Typed, constant, ifThenElse, (&&), (++), (==))
import Prelude          hiding ((&&), (++), (==))

-- | A definition of a state machine where some elements are defined
-- as streams.
--
-- A state machine is defined by an initial state, a final state, a no
-- transition stream (true when tere is no input coming in), a list of
-- transitions, and a bad state.
type StateMachine a = (a, a, Stream Bool, [(a, Stream Bool, a)], a)

-- | Produce a stream that, at any given time, contains the current state of
-- the state machine.
stateMachine :: forall a . (Eq a, Typed a) => StateMachine a -> Stream a
stateMachine (initial, final, noInputData, transitions, bad) = state
  where
    state         = ifThenElses transitions
    previousState = [initial] ++ state

    ifThenElses :: [(a, Stream Bool, a)] -> Stream a
    ifThenElses [] =
      ifThenElse (previousState == constant final && noInputData)
        (constant final)
        (constant bad)

    ifThenElses ((s1, i, s2):ss) =
      ifThenElse
        (previousState == constant s1 && i)
        (constant s2)
        (ifThenElses ss)

-- | Produce a stream that, at any given time, contains the current state of
-- the state machine as the numeric representation of an enum.
stateMachineEnum :: (Eq b, Typed b, Num b, Enum a)
                 => StateMachine a
                 -> Stream b
stateMachineEnum (initial, final, noInputData, transitions, bad) =
    stateMachine (fe initial, fe final, noInputData, transitionsE, fe bad)
  where
    transitionsE = map (\(s1, t, s2) -> (fe s1, t, fe s2)) transitions
    fe           = fromIntegral . fromEnum
