{-# LANGUAGE Rank2Types #-}

-- | The Copilot interpreter.
module Language.Copilot.Interpreter(interpretStreams) where

import Language.Copilot.Core

-- | The main function of this module.
-- It takes a /Copilot/ specification, the values of the monitored values,
-- and returns the values of the streams.
interpretStreams :: StreamableMaps Spec -> Vars -> Vars
interpretStreams streams moVs =
    inVs
    where 
        inVs = mapStreamableMaps (\ _ -> interpret inVs moVs) streams

interpret :: forall a. Streamable a => Vars -> Vars -> Spec a -> [a]
interpret inVs moVs s =
  case s of
    Const c -> repeat c
    Var v -> getElem v inVs
    PVar _ v _ -> checkV v (\v' -> (getElem v' moVs))
    PArr _ (v,s') _ -> checkV v (\v' -> map (\i ->    getElem v' moVs 
                                                   !! fromIntegral i)
                                            (interpret inVs moVs s'))
    Append ls s' -> ls ++ interpret inVs moVs s'
    Drop i s' -> drop i $ interpret inVs moVs s'
    F f _ s' -> map f (interpret inVs moVs s')
    F2 f _ s0 s1 -> zipWith f (interpret inVs moVs s0)
                              (interpret inVs moVs s1)
    F3 f _ s0 s1 s2 -> zipWith3 f 
                       (interpret inVs moVs s0) 
                       (interpret inVs moVs s1)
                       (interpret inVs moVs s2)
  where 
    checkV :: Ext -> (Var -> [a]) -> [a]
    checkV v f = 
          case v of
            ExtV v' -> f v'
            -- XXX support this?  
            Fun _ _ -> error $ "Sampling functions is not supported"
                               ++ " in the Copilot interpreter."
                          
