{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}

-- Note : for now, the initial state is computed during the first tick

-- | Transform the copilot specification in an atom one, and then compile that one.
module Language.Copilot.Compiler(copilotToAtom, tmpSampleStr) where

import Language.Copilot.Core

import Data.Maybe
import Data.Map as M
import Data.List
import Control.Monad (when)

import qualified Language.Atom as A

-- | Compiles an /Copilot/ specification to an /Atom/ one.
-- The period is given as a Maybe : if it is Nothing, an optimal period will be chosen.
copilotToAtom :: StreamableMaps Spec -> Sends -> Maybe Period 
              -> [(Var, String)] -> (Period, A.Atom ())
copilotToAtom streams sends p triggers = 
  (p', A.period p' $ do

    prophArrs <- mapStreamableMapsM initProphArr streams
    outputs <- mapStreamableMapsM initOutput streams

    updateIndexes <- foldStreamableMaps makeUpdateIndex prophArrs (return M.empty)
    outputIndexes <- foldStreamableMaps makeOutputIndex prophArrs (return M.empty)

    tmpSamples <- foldStreamableMaps (\_ -> initExtSamples streams prophArrs outputIndexes) 
                    streams 
                    (return emptyTmpSamples)

    -- One atom rule for each stream
    foldStreamableMaps (makeRule streams outputs prophArrs tmpSamples 
                           updateIndexes outputIndexes) 
      streams (return ())

    foldStreamableMaps (makeTrigger triggers streams prophArrs tmpSamples
                           outputIndexes)
      streams (return ())

    foldSendableMaps (makeSend outputs) sends (return ())

    -- Sampling of the external variables.  Remove redundancies.
    sequence_ $ snd . unzip $ nubBy (\x y -> fst x == fst y) $ 
      foldStreamableMaps (\_ -> sampleExts tmpSamples) streams []
    )
  where
    optP = getOptimalPeriod streams
    p' = 
      case p of
        Nothing -> optP
        Just i -> if i >= optP 
            then i 
            else error $ "Copilot error: the period is too short, " 
                     ++ "it should be at least " ++ show optP ++ " ticks."

initProphArr :: forall a. Streamable a => Var -> Spec a -> A.Atom (BoundedArray a)
initProphArr v s =
    let states = initState s
        name = "prophVal__" ++ normalizeVar v
        n = genericLength states in
    if n > 0
        then
            do
                array <- A.array name (states ++ [unit])
                -- unit is replaced by the good value during the first tick
                return $ B n $ Just array
    else return $ B n Nothing
    where
        initState s' =
            case s' of
                Append ls s'' -> ls ++ initState s''
                _ -> []

initOutput :: forall a. Streamable a => Var -> Spec a -> A.Atom (A.V a)
initOutput v _ = do
  atomConstructor ("outputVal__" ++ normalizeVar v) (unit::a)

tmpSampleStr :: String
tmpSampleStr = "tmpSampleVal__"

initExtSamples :: forall a. Streamable a 
               => StreamableMaps Spec -> ProphArrs -> Indexes -> Spec a 
                  -> A.Atom TmpSamples -> A.Atom TmpSamples
initExtSamples streams prophArrs outputIndexes s tmpSamples = do
    case s of
        Const _ -> tmpSamples
        Var _ ->   tmpSamples
        Drop _ s0 -> initExtSamples' s0 tmpSamples
        Append _ s0 -> initExtSamples' s0 tmpSamples
        F _ _ s0 -> initExtSamples' s0 tmpSamples
        F2 _ _ s0 s1 -> initExtSamples' s0 $
                           initExtSamples' s1 tmpSamples
        F3 _ _ s0 s1 s2 -> initExtSamples' s0 $ initExtSamples' s1 $
                             initExtSamples' s2 tmpSamples
        PVar _ v ph -> 
            do  checkVar v 
                ts <- tmpSamples
                let v' = tmpVarName v ph
                    vts = tmpVars ts
                    maybeElem = getMaybeElem v' vts::Maybe (PhasedValueVar a)
                    name = tmpSampleStr ++ normalizeVar v'
                case maybeElem of
                    Nothing -> 
                        do  val <- atomConstructor name (unit::a)
                            let m' = M.insert v' (PhV ph val) (getSubMap vts)
                            return $ ts {tmpVars = updateSubMap (\_ -> m') vts}
                    Just _ -> return ts
        PArr _ (arr, idx) ph -> 
            do  checkVar arr
                ts <- tmpSamples
                let arr' = tmpArrName arr ph (show idx)
                    arrts = tmpArrs ts
                    idxts = tmpIdxs ts
                    maybeElem = getMaybeElem arr' arrts::Maybe (PhasedValueArr a)
                    name = tmpSampleStr ++ normalizeVar arr'
                case maybeElem of 
                  Nothing -> -- if the array isn't in the map, neither is the index
                      do val <- atomConstructor name (unit::a)
                         let i = case idx of
                                   Const e -> PhIdx $ A.Const e
                                   Var _ -> 
--                                     let B initLen maybeArr = getElem v prophArrs 
--                                     let B initLen maybeArr = 
--                                           case getMaybeElem v prophArrs of
--                                             Nothing -> 
--                                               error "Error in function initExtSamples."
--                                             Just x -> x
                                     PhIdx $ nextSt streams prophArrs 
                                                undefined outputIndexes idx 0
                                   _    -> error "Unexpected Spec in initExtSamples."
                         let m' = M.insert arr' (PhA ph val) (getSubMap arrts)
                         let m'' = M.insert arr' i (getSubMap idxts)
                         return $ ts { tmpArrs = updateSubMap (\_ -> m') arrts
                                     , tmpIdxs = updateSubMap (\_ -> m'') idxts
                                     }
                  Just _ -> return ts
    where checkVar v = when (normalizeVar v /= v)
                       (error $ "Copilot: external variable " ++ v ++ " is not "
                               ++ "a valid C99 variable.")
          initExtSamples' :: Streamable b
                          => Spec b -> A.Atom TmpSamples -> A.Atom TmpSamples
          initExtSamples' = initExtSamples streams prophArrs outputIndexes

makeUpdateIndex :: Var -> BoundedArray a -> A.Atom Indexes -> A.Atom Indexes
makeUpdateIndex v (B n arr) indexes =
    case arr of
        Nothing -> indexes
        Just _ ->  
            do
                mindexes <- indexes
                index <- atomConstructor ("updateIndex__" ++ normalizeVar v) n
                return $ M.insert v index mindexes

makeOutputIndex :: Var -> BoundedArray a -> A.Atom Indexes -> A.Atom Indexes
makeOutputIndex v (B _ arr) indexes =
    case arr of
        Nothing -> indexes
        Just _ ->  
            do
                mindexes <- indexes
                index <- atomConstructor ("outputIndex__" ++ normalizeVar v) 0
                return $ M.insert v index mindexes

makeRule :: forall a. Streamable a => 
    StreamableMaps Spec -> Outputs -> ProphArrs -> TmpSamples -> 
    Indexes -> Indexes -> Var -> Spec a -> A.Atom () -> A.Atom ()
makeRule streams outputs prophArrs tmpSamples updateIndexes outputIndexes v s r = do
    r 
    let B n maybeArr = getElem v prophArrs::BoundedArray a
    case maybeArr of
        Nothing ->
            -- Fusing together the update and the output if the prophecy array doesn't exist 
            -- (ie if it would only have hold the output value)
            A.exactPhase 0 $ A.atom ("updateOutput__" ++ normalizeVar v) $ do
                ((getElem v outputs)::(A.V a)) A.<== nextSt'

        Just arr -> do
            let updateIndex = fromJust $ M.lookup v updateIndexes
                outputIndex = fromJust $ M.lookup v outputIndexes

            A.exactPhase 0 $ A.atom ("update__" ++ normalizeVar v) $ do
                arr A.! (A.VRef updateIndex) A.<== nextSt'
            
            A.exactPhase 1 $ A.atom ("output__" ++ normalizeVar v) $ do
                ((getElem v outputs)::(A.V a)) A.<== arr A.!. (A.VRef outputIndex)
                outputIndex A.<== (A.VRef outputIndex + A.Const 1) `A.mod_` A.Const (n + 1)
            
            -- Spread these out evenly accross the remaining phases, staring no
            -- earlier than phase 1.
            A.phase ((maxSampleDep v streams) + 1)
              $ A.atom ("incrUpdateIndex__" ++ normalizeVar v) $ do
                updateIndex A.<== (A.VRef updateIndex + A.Const 1) `A.mod_` A.Const (n + 1)

       where nextSt' = nextSt streams prophArrs tmpSamples outputIndexes s 0
             
-- | Find the maximum phase as which an array sampling depends on this stream by
-- computing it's index in terms of it. Returns zero by default.
maxSampleDep :: Var -> StreamableMaps Spec -> Int
maxSampleDep v streams =
  foldStreamableMaps (\_ -> streamDep) streams 0
  where 
    streamDep :: Streamable b => Spec b -> Int -> Int
    streamDep s i = 
      case s of
        Var _ -> i
        Const _  -> i
        PVar _ _ _ -> i
        PArr _ (_, Var v') ph | v == v'   -> max ph i
                              | otherwise -> i
        PArr _ _ _ -> i
        F _ _ s0 -> streamDep s0 i
        F2 _ _ s0 s1 -> streamDep s0 $ streamDep s1 i
        F3 _ _ s0 s1 s2 -> streamDep s0 $ streamDep s1 $ streamDep s2 i
        Append _ s0 -> streamDep s0 i
        Drop _ s0 -> streamDep s0 i

makeSend :: forall a. Sendable a => Outputs -> Var -> Send a -> A.Atom () -> A.Atom ()
makeSend outputs name (Send (v, ph, port)) r = do
        r 
        A.exactPhase ph $ A.atom ("__send_" ++ name) $
            send ((A.value (getElem v outputs))::(A.E a)) port

-- What we really should be doing is just folding over the TmpSamples, since
-- that data should contain all the info we need to construct external variable
-- and external array samples.  However, there is the issue that for array
-- samples, the type of the index may differ from the type of the array, and
-- having the spec available provides typing coercion.  We could fold over the
-- TmpSamples, passing streams in, and extract the appropriate Spec a.
sampleExts :: forall a. Streamable a 
           => TmpSamples -> Spec a -> [(Var, A.Atom ())] -> [(Var, A.Atom ())]
sampleExts ts s a = do
  case s of
    Var _ -> a
    Const _ -> a
    PVar _ v ph -> 
     let v' = tmpVarName v ph 
         PhV _ var = getElem v' (tmpVars ts)::PhasedValueVar a in
     (v', A.exactPhase ph $ 
            A.atom ("sample__" ++ v') $ 
              var A.<== (A.value $ externalAtomConstructor v)
     ) : a

    PArr _ (arr, idx) ph -> 
         let arr' = tmpArrName arr ph (show idx)
             PhIdx i = getIdx arr' idx (tmpIdxs ts)
--             PhA _ arrV = getElem arr' (tmpArrs ts)::PhasedValueArr a in
             PhA _ arrV = case getMaybeElem arr' (tmpArrs ts)::Maybe (PhasedValueArr a) of
                            Nothing -> error "Error in fucntion sampleExts."
                            Just x -> x
         in 
     (arr', A.exactPhase ph $ 
              A.atom ("sample__" ++ arr') $ 
                arrV A.<== A.array' arr (atomType (unit::a)) A.!. i
     ) : a
    F _ _ s0 -> sampleExts ts s0 a
    F2 _ _ s0 s1 -> sampleExts ts s0 $ sampleExts ts s1 a
    F3 _ _ s0 s1 s2 -> sampleExts ts s0 $ sampleExts ts s1 $
                         sampleExts ts s2 a
    Append _ s0 -> sampleExts ts s0 a
    Drop _ s0 -> sampleExts ts s0 a

-- lookup the idx for external array accesses in the map.
getIdx :: forall a. (Streamable a, A.IntegralE a) 
       => Var -> Spec a -> StreamableMaps PhasedValueIdx -> PhasedValueIdx a
getIdx arr s ts = 
  case s of
    Var _   -> case getMaybeElem arr ts of
                 Nothing -> error "Error in function getIdx."
                 Just x  -> x
    Const e -> PhIdx $ A.Const e
    _       -> error $ "Expecing either a variable or constant for the index "
                 ++ "in the external array access for array " ++ arr ++ "."

getOptimalPeriod :: StreamableMaps Spec -> Period
getOptimalPeriod streams =
  foldStreamableMaps getMaximumSamplingPhase streams 2
  where
    getMaximumSamplingPhase :: Var -> Spec a -> Period -> Period 
    getMaximumSamplingPhase _ spec n =
      case spec of
        PVar _ _ ph -> max (ph + 1) n 
        PArr _ (_, Var _) ph -> max (ph + 2) n -- because this may depend on a
                                               -- variable, and if that variable
                                               -- has a prophecy array, it needs
                                               -- an extra phase to update after
                                               -- the index is taken.
        PArr _ _ ph -> max (ph + 1) n
        F _ _ s -> getMaximumSamplingPhase "" s n
        F2 _ _ s0 s1 -> maximum [n,
                (getMaximumSamplingPhase "" s0 n),
                (getMaximumSamplingPhase "" s1 n)]
        F3 _ _ s0 s1 s2 -> maximum [n,
                (getMaximumSamplingPhase "" s0 n), 
                (getMaximumSamplingPhase "" s1 n), 
                (getMaximumSamplingPhase "" s2 n)]
        Drop _ s -> getMaximumSamplingPhase "" s n
        Append _ s -> getMaximumSamplingPhase "" s n
        _ -> n
