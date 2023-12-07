{-# LANGUAGE RankNTypes #-}
-- | Graphs
module CopilotData.Data.Graph where

import qualified Data.Graph as G

-- | Graphs that are polymorphic on the type of the state.
data Graph state = Graph
  { graphGraph      :: G.Graph
  , graphStates     :: [state]
  , graphAdjacency  :: [(state, state, [state])]
  , graphStateKeys  :: state -> Maybe G.Vertex
  }

-- | True if two states are connected in the graph.
graphPath :: Ord state
          => Graph state
          -> state
          -> state
          -> Bool
graphPath g s1 s2
  | Just s1' <- graphStateKeys g s1
  , Just s2' <- graphStateKeys g s2
  = G.path (graphGraph g) s1' s2'

  | otherwise
  = False

-- | List all strongly connected components.
graphSCCs :: forall state
           . Ord state
          => Graph state
          -> [Graph state]
graphSCCs graph = map (subgraph graph) $ map G.flattenSCC sccs
  where
    sccs = G.stronglyConnComp $ graphAdjacency graph

-- | Narrow a graph to only contain specific states.
subgraph :: Ord state
         => Graph state
         -> [state]
         -> Graph state
subgraph graph states = Graph g s a k
  where
    g = graphGraph graph
    s = states
    a = [ (v, v, vs) | (v, _, vs0) <- graphAdjacency graph
                     , v `elem` states
                     , let vs = filter (`elem` states) vs0
                     , not (null vs)
        ]
    k s
      | s `elem` states = graphStateKeys graph s
      | otherwise       = Nothing
