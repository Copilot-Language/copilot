{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}

-- Note : for now, the initial state is computed during the first tick

-- | Transform the copilot specification in an atom one, and then compile that one.
module Language.Copilot.Compiler(copilotToAtom) where

import Language.Copilot.Core

import Data.Word
import Data.Maybe
import Data.Map as M
import Data.List
import Control.Monad (when) 

import qualified Language.Atom as A
import Language.Copilot.Analyser (getAtomType)

getValue :: PhasedValue a -> A.V a
getValue (Ph _ val) = val

-- | Compiles an /Copilot/ specification to an /Atom/ one.
--
-- The period is given as a Maybe : if it is Nothing, an optimal period will be chosen.
copilotToAtom :: StreamableMaps Spec -> Sends -> Maybe Period -> Maybe [(Var, String)] -> (Period, A.Atom ())
copilotToAtom streams sends p triggers = 
  (p', A.period p' $ do
    -- Three streamableMaps : 
    -- prophecy arrays (Int, A.A a), outputs (A.V a), temporary samples (Phase, A.V a)
    prophArrs <- mapStreamableMapsM initProphArr streams
    outputs <- mapStreamableMapsM initOutput streams
    tmpSamples <- foldStreamableMaps initTmpSamples streams (return emptySM)
    updateIndexes <- foldStreamableMaps makeUpdateIndex prophArrs (return M.empty)
    outputIndexes <- foldStreamableMaps makeOutputIndex prophArrs (return M.empty)

    -- One atom rule for each stream
    foldStreamableMaps (makeRule streams outputs prophArrs tmpSamples 
                           updateIndexes outputIndexes) 
      streams (return ())

    foldStreamableMaps (makeTrigger triggers streams prophArrs tmpSamples
                           outputIndexes)
      streams (return ())

    foldSendableMaps (makeSend outputs) sends (return ())

    -- Sampling of the external variables
    sampleVars tmpSamples)
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
initOutput v s = streamToUnitValue ("outputVal__" ++ normalizeVar v) s

initTmpSamples :: forall a. Streamable a => Var -> Spec a -> A.Atom TmpSamples -> A.Atom TmpSamples
initTmpSamples _ s tmpSamples =
    case s of
        Const _ -> tmpSamples
        Var _ -> tmpSamples
        Drop _ s' -> initTmpSamples undefined s' tmpSamples
        Append _ s' -> initTmpSamples undefined s' tmpSamples
        F _ _ s0 -> initTmpSamples undefined s0 tmpSamples
        F2 _ _ s0 s1 -> initTmpSamples undefined s0 $
            initTmpSamples undefined s1 tmpSamples
        F3 _ _ s0 s1 s2 -> initTmpSamples undefined s0 $
            initTmpSamples undefined s1 $
            initTmpSamples undefined s2 tmpSamples
        PVar _ v ph -> 
            do
                ts <- tmpSamples
                let v' = normalizeVar v ++ "_" ++ show ph
                    maybeElem = (getMaybeElem v' ts)::(Maybe (PhasedValue a))
                    name = "tmpSampleVal__" ++ normalizeVar v'
                case maybeElem of
                    Nothing -> 
                        do
                            val <- streamToUnitValue name s
                            let m' = M.insert v' (Ph ph val) (getSubMap ts)
                            return $ updateSubMap (\_ -> m') ts
                    Just _ -> return ts -- to avoid duplicates. 
                        -- Beware : crashes if "tmpSamples" instead of "return ts" 

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
    let B n maybeArr = (getElem v prophArrs)::(BoundedArray a)
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
            
            A.phase 1 $ A.atom ("incrUpdateIndex__" ++ normalizeVar v) $ do
                updateIndex A.<== (A.VRef updateIndex + A.Const 1) `A.mod_` A.Const (n + 1)

--            A.phase 2 $ A.atom ("incrOutputIndex__" ++ normalizeVar v) $ do


       where nextSt' = nextSt streams prophArrs tmpSamples outputIndexes s 0

makeSend :: forall a. Sendable a => Outputs -> Var -> Send a -> A.Atom () -> A.Atom ()
makeSend outputs name (Send (v, ph, port)) r =
    r >>
    do
        A.exactPhase ph $ A.atom ("__send_" ++ name) $
            send ((A.value (getElem v outputs))::(A.E a)) port

sampleVars :: TmpSamples -> A.Atom ()
sampleVars tmpSamples = 
    sequence_ $ foldStreamableMaps sample tmpSamples []
    where
        sample :: Streamable a => Var -> PhasedValue a -> [A.Atom ()] -> [A.Atom ()]
        sample v' (Ph ph val) xs = 
            (A.exactPhase ph $ 
                A.atom ("sample__" ++ normalizeVar v') $ 
                val A.<== (A.value $ externalAtomConstructor v)
                ) : xs
            where
                -- TODO : fix this mess
                v0 = reverse (tail (dropWhile (\c -> c /= '_') (reverse v')))
                (v1, n_final) = foldl (\ (s, n) c -> 
                        if c == '_' 
                            then (s, n + 1) 
                            else (s ++ underscoreNumber2Symbol n ++ [c],0)
                        ) ("", 0) v0
                v = v1 ++ underscoreNumber2Symbol n_final
                underscoreNumber2Symbol 0 = ""
                underscoreNumber2Symbol 1 = "_"
                underscoreNumber2Symbol 2 = "."
                underscoreNumber2Symbol 3 = "["
                underscoreNumber2Symbol 4 = "]"
                underscoreNumber2Symbol _ = undefined

getOptimalPeriod :: StreamableMaps Spec -> Period
getOptimalPeriod streams =
  foldStreamableMaps getMaximumSamplingPhase streams 2
  where
    getMaximumSamplingPhase :: Var -> Spec a -> Period -> Period 
    getMaximumSamplingPhase _ spec n =
      case spec of
        PVar _ _ p -> max (p + 1) n
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
