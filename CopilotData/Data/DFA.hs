{-# LANGUAGE RankNTypes #-}
module CopilotData.Data.DFA where

import Data.List  ( sort, nub )
import Data.Maybe ( fromJust )
import Prelude    as P
import           System.IO
import           System.IO.Unsafe
import           Text.Pretty.Simple  (pPrint)

import CopilotData.Data.NFA

prettyDebug prefix a = unsafePerformIO $ do
  print prefix
  pPrint a
  hFlush stdout

test = powerset nfa

origin :: IO ()
origin = pPrint nfa

nfa = NFA
  [0, 1]
  ["zero", "one"]
  "zero"
  [ (("zero", 0),["zero"])
  , (("zero", 1),["one"])
  , (("one", 0), ["one"])
  , (("one", 1), ["one"])
  ]
  ["one"]

nfa2 = NFA
    { nfaAlphabet =
        [ 0
        , 1
        ]
    , nfaStates =
        [ 1
        , 0
        ]
    , nfaInitialState = 0
    , nfaTransitions =
        [
            ( ( 1 , 0) , [ 0 ])
        ,
            ( ( 1 , 1) , [ 1 ])
        ,
            ( ( 0 , 0) , [ 1 ])
        ,
            ( ( 0 , 1) , [ 0 ])
        ]
    , nfaFinalStates = []
    }

-- * Deterministic Finite Automata

data DFA alphabet state = DFA
  { alphabet     :: [alphabet]
  , states       :: [state]
  , initialState :: state
  , transitions  :: [((state, alphabet), state)]
  , finalStates  :: [state]
  }
  deriving (Eq, Show)

powerset :: forall alphabet state
          . (Eq alphabet, Ord state, Show state, Show alphabet)
         => NFA alphabet state
         -> DFA alphabet [state]
powerset nfa = powerset' nfa front s' r'
  where
    front = [[nfaInitialState nfa]]
    s'    = []
    r'    = []

powerset' :: forall alphabet state
           . (Eq alphabet, Ord state, Show state, Show alphabet)
          => NFA alphabet state
          -> [[state]]
          -> [[state]]
          -> [(([state], alphabet), [state])]
          -> DFA alphabet [state]
powerset' nfa [] s' r' = DFA a s i t f
 where
   -- a :: [alphabet] -- doesn't type check but is correct?
   a = nub $ nfaAlphabet nfa

   s = nub s'

   i = [nfaInitialState nfa]

   -- t :: ([state], alphabet) -> [state]
   t = nub r'

   f = filter (any (`elem` (nfaFinalStates nfa))) s'

powerset' nfa (f1:front) s' r' = powerset'' nfa f1 front s'' r' (nfaAlphabet nfa)
  where
    s'' = nub $ s' P.++ [f1]

powerset'' :: forall alphabet state
            . (Eq alphabet, Ord state, Show state, Show alphabet)
           => NFA alphabet state
           -> [state]
           -> [[state]]
           -> [[state]]
           -> [(([state], alphabet), [state])]
           -> [alphabet]
           -> DFA alphabet [state]
powerset'' nfa s front sTilde rTilde [] = powerset' nfa front sTilde rTilde

powerset'' nfa s front sTilde rTilde (sigma:uSigma) = powerset'' nfa s front' sTilde rTilde' uSigma
  where
    rTilde' = rTilde P.++ [((s, sigma), s')]
    s'      = nub $ sort $ concatMap (\state -> mySuccessors nfa state sigma) s
    front'  = if s' `notElem` sTilde then front P.++ [s'] else front

mySuccessors nfa state sigma =
  let e = successors nfa state sigma
  in -- (prettyDebug "mySucc:" (state, sigma, e)) `seq` e
     e

myFromJust Nothing = error "DFAFromJust"
myFromJust x = fromJust x
