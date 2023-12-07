module CopilotData.Data.State where

import Prelude hiding ( mod, (&&), (++), (==), (/=) )

import Language.Copilot

-- Initial state, final state, no transition signal, transitions, bad state
type StateMachineGF =
  (Word8, Word8, Stream Bool, [(Word8, Stream Bool, Word8)], Word8, [(Word8, Word8)])

stateMachineGF :: StateMachineGF -> Stream Word8
stateMachineGF (initialState, finalState, noInputData, transitions, badState, outputMapping) =
    mapOutput 255 state outputMapping
  where
    state = [initialState] ++ ifThenElses transitions

    ifThenElses :: [(Word8, Stream Bool, Word8)] -> Stream Word8
    ifThenElses [] =
      ifThenElse (state == constant finalState && noInputData)
        (constant finalState)
        (constant badState)

    ifThenElses ((s1, i, s2):ss) =
      ifThenElse (state == constant s1 && i) (constant s2) (ifThenElses ss)

-- | Apply a map to a stream.
mapOutput :: Word8 -> Stream Word8 -> [(Word8, Word8)] -> Stream Word8
mapOutput def s []          = constant def
mapOutput def s ((k, v):xs) =
  ifThenElse (s == constant k)
             (constant v)
             (mapOutput def s xs)

input = extern "inputs" $ Just [1, 2, 2, 2, 1, 1]

-- Using our API
stateMachine1GF :: Stream Word8
stateMachine1GF = stateMachineGF (initialState, finalState, noInputs, transitions, badState, mapping)
  where
    initialState :: Word8
    initialState = 1

    finalState :: Word8
    finalState = 4

    noInputs :: Stream Bool
    noInputs = noneOf input [1, 2]

    transitions = [ (1, input == 1, 2)
                  , (2, input == 2, 3)
                  , (3, input == 2, 3)
                  , (3, input == 1, 4)
                  ]

    badState :: Word8
    badState = 5

    mapping = map (\x -> (x, x)) [1..5]

noneOf :: Stream Word8 -> [Word8] -> Stream Bool
noneOf input [] = true
noneOf input (x:xs) = input /= constant x && noneOf input xs

testGraph1GF :: IO ()
testGraph1GF = do
  interpret 6 spec

spec :: Spec
spec = do
  trigger "values" true [ arg stateMachine1GF ]
