-- | Non-deterministic Finite Automata
module CopilotData.Data.NFA where

import Data.List  ( lookup )
import Data.Maybe ( fromMaybe )

-- * Non-deterministic Finite Automata
data NFA alphabet state = NFA
  { nfaAlphabet     :: [alphabet]
  , nfaStates       :: [state]
  , nfaInitialState :: state
  , nfaTransitions  :: [((state, alphabet), [state])]
  , nfaFinalStates  :: [state]
  }
  deriving (Eq, Show)

successors :: (Eq alphabet, Eq state)
           => NFA alphabet state -> state -> alphabet -> [state]
successors nfa state alphabet = concat
                              $ map snd
                              $ filter (((state, alphabet) ==).fst)
                              $ nfaTransitions nfa
