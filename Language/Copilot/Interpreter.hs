{-# LANGUAGE Rank2Types #-}

-- | The Copilot interpreter.
module Language.Copilot.Interpreter
    (interpretStreams) where

import Language.Copilot.Core

-- | The main function of this module.
-- It takes a /Copilot/ specification, the values of the monitored values,
-- and returns the values of the streams.
interpretStreams :: StreamableMaps Spec -> Vars -> Vars
interpretStreams streams extVs = copilotVs
  where copilotVs = mapStreamableMaps (\ _ -> interpret copilotVs extVs) streams

interpret :: forall a. Streamable a => Vars -> Vars -> Spec a -> [a]
interpret copilotVs extVs s =
  case s of
    Const c -> repeat c
    Var v -> getElem v copilotVs
    ExtVar _ v -> checkV v (\v' -> (getElem v' extVs))
    ExtArr _ (v,s') -> checkV v (\v' -> map (\i ->    getElem v' extVs 
                                                 !! fromIntegral i)
                                            (interpret copilotVs extVs s'))
    Append ls s' -> ls ++ interpret copilotVs extVs s'
    Drop i s' -> drop i $ interpret copilotVs extVs s'
    F f _ s' -> map f (interpret copilotVs extVs s')
    F2 f _ s0 s1 -> zipWith f (interpret copilotVs extVs s0)
                              (interpret copilotVs extVs s1)
    F3 f _ s0 s1 s2 -> zipWith3 f 
                       (interpret copilotVs extVs s0) 
                       (interpret copilotVs extVs s1)
                       (interpret copilotVs extVs s2)
  where 
    checkV :: Ext -> (Var -> [a]) -> [a]
    checkV v f = 
          case v of
            ExtV v' -> f v'
            -- XXX support this?  
            Fun _ _ -> error $ "Sampling functions is not currently supported"
                               ++ " in the Copilot interpreter."
                          
