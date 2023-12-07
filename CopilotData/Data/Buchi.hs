module CopilotData.Data.Buchi where

-- External
import qualified Data.Graph  as G
import           Data.List   ( intersect, sort )
import           Data.Maybe  ( catMaybes )
import           Prelude     hiding ( mod, (&&), (++), (/=), (<), (==), (>) )
import qualified Prelude     as P

-- External: Copilot
import Language.Copilot hiding ( not )

-- Temporary
import Debug.Trace

-- Internal
import CopilotData.Data.Graph

-- * Non-deterministic Buchi automata

data NBA alphabet state = NBA
  { nbaAlphabet     :: [alphabet]
  , nbaInitialState :: state
  , nbaState        :: [state]
  , nbaTransitions  :: [((state, alphabet), [state])]
  , nbaFinalStates  :: [state]
  }
  deriving (Eq, Show)

isLanguageEmpty :: (Eq alphabet, Ord state) => NBA alphabet state -> Bool
isLanguageEmpty nba = not
                    $ null
                    $ intersect
                        (nbaFinalStates nba)
                        (stronglyConnected nba (nbaInitialState nba))

stronglyConnected :: (Eq alphabet, Ord state) => NBA alphabet state -> state -> [state]
stronglyConnected nba s = sort $ concatMap graphStates $ filter (reachable nba s) graphs
  where
    graphs = stronglyConnectedComponents nba

stronglyConnectedComponents :: (Eq alphabet, Ord state) => NBA alphabet state -> [Graph state]
stronglyConnectedComponents nba = graphSCCs (nbaToGraph nba)

reachable :: (Eq alphabet, Ord state)
          => NBA alphabet state
          -> state
          -> Graph state
          -> Bool
reachable nba state graph = any (nbaReachable nba state) (graphStates graph)

nbaReachable :: (Eq alphabet, Ord state) => NBA alphabet state -> state -> state -> Bool
nbaReachable nba s1 s2 = graphPath (nbaToGraph nba) s1 s2

nbaToGraph :: (Eq alphabet, Ord state) => NBA alphabet state -> Graph state
nbaToGraph nba = Graph g (nbaState nba) adjacents kf
  where
    (g, vf, kf) = (G.graphFromEdges adjacents)
    adjacents = [ (s, s, ss)
                | s <- nbaState nba
                , let ss = sort $ concat $ catMaybes $ map (\a -> lookup (s, a) (nbaTransitions nba)) (nbaAlphabet nba)
                ]
