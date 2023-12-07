-- * Finite State Machines
module CopilotData.Data.FSM where

import Text.Pretty.Simple (pPrint)

-- | Finite State Machines
data FSM alphabet state output = FSM
  { fsmAlphabet       :: [alphabet]
  , fsmStates         :: [state]
  , fsmInitialState   :: state
  , fsmTransitions    :: [((state, alphabet), state)]
  , fsmOutputs        :: [output]
  , fsmOutputFunction :: [(state, output)]
  }
  deriving (Eq, Show)

minimize :: FSM alphabet state output -> FSM alphabet state output
minimize = id
