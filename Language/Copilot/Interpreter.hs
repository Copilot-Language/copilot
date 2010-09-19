-- | This interpreter is mostly used for checking the /Copilot/ implementation.
-- But it can also be used for quick testing of a /Copilot/ design
module Language.Copilot.Interpreter(interpretStreams) where

import Language.Copilot.Core

import Data.List 
import Prelude

-- | The main function of this module.
-- It takes a /Copilot/ specification, the values of the monitored values,
-- and returns the values of the streams.
interpretStreams :: StreamableMaps Spec -> Vars -> Vars
interpretStreams streams moVs =
    inVs
    where 
        inVs = mapStreamableMaps (\ _ -> interpret inVs moVs) streams

interpret :: Streamable a => Vars -> Vars -> Spec a -> [a]
interpret inVs moVs s =
        case s of
            Const c -> repeat c
            Var v -> getElem v inVs
            PVar _ v _ -> getElem v moVs
            Append ls s' -> ls ++ interpret inVs moVs s'
            Drop i s' -> drop i $ interpret inVs moVs s'
            F f _ s' -> map f (interpret inVs moVs s')
            F2 f _ s0 s1 -> zipWith f 
                (interpret inVs moVs s0)
                (interpret inVs moVs s1)
            F3 f _ s0 s1 s2 -> zipWith3 f 
                (interpret inVs moVs s0) 
                (interpret inVs moVs s1)
                (interpret inVs moVs s2)
