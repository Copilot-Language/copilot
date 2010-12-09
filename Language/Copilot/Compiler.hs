{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}

-- XXX Clean this mess up!

-- | Transform the copilot specification in an atom one, and then compile that one.
module Language.Copilot.Compiler(copilotToAtom, tmpSampleStr, tmpArrName, tmpVarName) where

import Language.Copilot.Core

import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.Word (Word64)

import qualified Language.Atom as A

-- | Compiles an /Copilot/ specification to an /Atom/ one.
-- The period is given as a Maybe : if it is Nothing, an optimal period will be chosen.
copilotToAtom :: LangElems -> Maybe Period -> (Period, A.Atom ())
copilotToAtom (LangElems streams sends triggers) p = 
  (p', A.period p' $ do
    prophArrs <- mapStreamableMapsM initProphArr streams
    outputs <- mapStreamableMapsM initOutput streams
    updateIndexes <- foldStreamableMaps makeUpdateIndex prophArrs (return M.empty)
    outputIndexes <- foldStreamableMaps makeOutputIndex prophArrs (return M.empty)
    tmpSamples <- foldStreamableMaps 
                    (\_ -> initExtSamples streams prophArrs outputIndexes) 
                    streams 
                    (return emptyTmpSamples)
    -- One atom rule for each stream
    foldStreamableMaps (makeRule p' streams outputs prophArrs tmpSamples 
                           updateIndexes outputIndexes) 
      streams (return ())

    M.fold (makeTrigger outputs) (return ()) triggers
           
    foldStreamableMaps (makeSend outputs) sends (return ())

    -- Sampling of the external variables.  Remove redundancies.
    sequence_ $ snd . unzip $ nubBy (\x y -> fst x == fst y) $ 
      foldStreamableMaps (\_ -> sampleExts tmpSamples) streams []
    )
  where p' = period p
--  where
--    optP = getOptimalPeriod streams sends

-- | For period of length n:
-- Phase 0: state update.
-- Phase 1: Compute output variables.
-- Phase 2 - n-2: send values (if any).
-- Phase 2 - n-2: Sample external vars (if any).
-- Phase n-1: update indexes.
period :: Maybe Int -> Int
period p = 
  case p of
    Nothing -> minPeriod
    Just i -> if i >= minPeriod
                then i 
                else error $ "Copilot error: the period is too short, " 
                       ++ "it should be at least " ++ show minPeriod ++ " ticks."
  where minPeriod :: Int
        minPeriod = 4

-- For the prophecy arrays
type ArrIndex = Word64
type ProphArrs = StreamableMaps BoundedArray
type Outputs = StreamableMaps A.V
type Indexes = M.Map Var (A.V ArrIndex)

-- External variables
data PhasedValueVar a = PhV (A.V a)
--type TmpVarSamples = StreamableMaps PhasedValueVar

data BoundedArray a = B ArrIndex (Maybe (A.A a))

nextSt :: Streamable a => StreamableMaps Spec -> ProphArrs -> TmpSamples -> Indexes 
       -> Spec a -> ArrIndex -> A.E a
nextSt streams prophArrs tmpSamples outputIndexes s index = 
    case s of
        PVar _ v  -> 
          let PhV var = getElem (tmpVarName v) (tmpVars tmpSamples) in
          A.value var
        PArr _ (v, idx) -> 
          let PhA var = e tmp (tmpArrs tmpSamples) 
              tmp = tmpArrName v (show idx) 
              e a b = case getMaybeElem a b of
                        Nothing -> 
                          error "Error in application of getElem in nextSt."
                        Just x  -> x 
          in A.value var
        Var v -> let B initLen maybeArr = getElem v prophArrs in
            -- This check is extremely important
            -- It means that if x at time n depends on y at time n
            -- then x is obtained not by y, but by inlining the definition of y
            -- so it increases the size of code (sadly),
            -- but is the only thing preventing race conditions from occuring
            if index < initLen
                then getVar v initLen maybeArr 
                else let s0 = getElem v streams in 
                     next s0 (index - initLen)
        Const e -> A.Const e
        F _ f s0 -> f $ next s0 index
        F2 _ f s0 s1 ->
            f (next s0 index) 
              (next s1 index)
        F3 _ f s0 s1 s2 ->
            f (next s0 index) 
              (next s1 index) 
              (next s2 index)
        Append _ s0 -> next s0 index
        Drop i s0 -> next s0 (fromInteger (toInteger i) + index)
    where
        next :: Streamable b => Spec b -> ArrIndex -> A.E b
        next = nextSt streams prophArrs tmpSamples outputIndexes 
        getVar :: Streamable a 
               => Var -> ArrIndex -> Maybe (A.A a) -> A.E a
        getVar v initLen maybeArr =
           let outputIndex = case M.lookup v outputIndexes of
                               Nothing -> error "Error in function getVar."
                               Just x -> x
               arr = case maybeArr of
                       Nothing -> error "Error in function getVar (maybeArr)."
                       Just x -> x in 
           arr A.!. ((A.Const index + A.VRef outputIndex) `A.mod_`  
                       (A.Const (initLen + 1)))

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


-- External arrays
data PhasedValueArr a = PhA (A.V a) -- Array name.
data PhasedValueIdx a = PhIdx (A.E a) -- variable that gives index. 

data TmpSamples = 
  TmpSamples { tmpVars :: StreamableMaps PhasedValueVar
             , tmpArrs :: StreamableMaps PhasedValueArr
             , tmpIdxs :: StreamableMaps PhasedValueIdx
             }

emptyTmpSamples :: TmpSamples
emptyTmpSamples = TmpSamples emptySM emptySM emptySM

tmpVarName :: Ext -> Var
tmpVarName v = show v -- ++ "_" ++ show ph

tmpArrName :: Ext -> String -> Var
tmpArrName v idx = (tmpVarName v) ++ "_" ++ normalizeVar idx

initOutput :: forall a. Streamable a => Var -> Spec a -> A.Atom (A.V a)
initOutput v _ = do
  atomConstructor (normalizeVar v) (unit::a)

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
        PVar _ v -> 
            do  -- checkVar v 
                ts <- tmpSamples
                let v' = tmpVarName v 
                    vts = tmpVars ts
                    maybeElem = getMaybeElem v' vts::Maybe (PhasedValueVar a)
                    name = tmpSampleStr ++ normalizeVar v'
                case maybeElem of
                    Nothing -> 
                        do  val <- atomConstructor name (unit::a)
                            let m' = M.insert v' (PhV val) (getSubMap vts)
                            return $ ts {tmpVars = updateSubMap (\_ -> m') vts}
                    Just _ -> return ts
        PArr _ (arr, idx) -> 
            do  --checkVar arr
                ts <- tmpSamples
                let arr' = tmpArrName arr (show idx)
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
                         let m' = M.insert arr' (PhA val) (getSubMap arrts)
                         let m'' = M.insert arr' i (getSubMap idxts)
                         return $ ts { tmpArrs = updateSubMap (\_ -> m') arrts
                                     , tmpIdxs = updateSubMap (\_ -> m'') idxts
                                     }
                  Just _ -> return ts
    where 
    --      checkVar v = when (normalizeVar v /= v)
    --                    (error $ "Copilot: external variable " ++ v ++ " is not "
    --                            ++ "a valid C99 variable.")
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
    Period -> StreamableMaps Spec -> Outputs -> ProphArrs -> TmpSamples -> 
    Indexes -> Indexes -> Var -> Spec a -> A.Atom () -> A.Atom ()
makeRule p streams outputs prophArrs tmpSamples updateIndexes outputIndexes v s r = do
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
                outputIndex A.<==          (A.VRef outputIndex + A.Const 1) 
                                  `A.mod_` A.Const (n + 1)
            
            -- XXX For now, we put these all in the last phase.  We want better
            -- distribution though at some point.
            A.phase (p - 1)
              $ A.atom ("incrUpdateIndex__" ++ normalizeVar v) $ do
                updateIndex A.<==          (A.VRef updateIndex + A.Const 1) 
                                  `A.mod_` A.Const (n + 1)

       where nextSt' = nextSt streams prophArrs tmpSamples outputIndexes s 0
             
-- -- | Find the maximum phase as which an array sampling depends on this stream by
-- -- computing it's index in terms of it. Returns zero by default.
-- maxSampleDep :: Var -> StreamableMaps Spec -> Int
-- maxSampleDep v streams =
--   foldStreamableMaps (\_ -> streamDep) streams 0
--   where 
--     streamDep :: Streamable b => Spec b -> Int -> Int
--     streamDep s i = 
--       case s of
--         Var _ -> i
--         Const _  -> i
--         PVar _ _ _ -> i
--         PArr _ (_, Var v') | v == v'   -> max ph i
--                            | otherwise -> i
--         PArr _ _ _ -> i
--         F _ _ s0 -> streamDep s0 i
--         F2 _ _ s0 s1 -> streamDep s0 $ streamDep s1 i
--         F3 _ _ s0 s1 s2 -> streamDep s0 $ streamDep s1 $ streamDep s2 i
--         Append _ s0 -> streamDep s0 i
--         Drop _ s0 -> streamDep s0 i

-- Do sends in phase 2.
makeSend :: forall a. Streamable a 
         => Outputs -> String -> Send a -> A.Atom () -> A.Atom ()
makeSend outputs name (Send v port portName) r = do
        r 
        A.exactPhase 2 $ A.atom ("__send_" ++ name) $
            mkSend (A.value (notVarErr v (\var -> getElem var outputs)) :: A.E a) 
                   port 
                   portName

-- | Sending data over ports.
mkSend :: (Streamable a) => A.E a -> Port -> String -> A.Atom ()
mkSend e (Port port) portName =
  A.action (\[ueStr] -> portName ++ "(" ++ ueStr ++ "," ++ show port ++ ")") 
           [A.ue e]

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
    PVar _ v -> 
     let v' = tmpVarName v
         PhV var = getElem v' (tmpVars ts)::PhasedValueVar a in
     (v', A.exactPhase minSampPh $ 
            A.atom ("sample__" ++ v') $ 
              var A.<== (A.value $ case v of
                                     ExtV extV -> externalAtomConstructor extV
                                     Fun nm args -> undefined
                        )
     ) : a
    PArr _ (arr, idx) -> 
         let arr' = tmpArrName arr (show idx)
             PhIdx i = getIdx arr' idx (tmpIdxs ts)
             PhA arrV = 
               case getMaybeElem arr' (tmpArrs ts)::Maybe (PhasedValueArr a) of
                 Nothing -> error "Error in fucntion sampleExts."
                 Just x -> x
         in (arr', A.exactPhase minSampPh $ 
              A.atom ("sample__" ++ arr') $ 
                arrV A.<== A.array' (case arr of
                                       ExtV extV -> extV
                                       Fun _ _ -> error "Still need to implement in sampleExts in Compiler.hs." -- XXX
                                    ) (atomType (unit::a)) A.!. i
            ) : a
    F _ _ s0 -> sampleExts ts s0 a
    F2 _ _ s0 s1 -> sampleExts ts s0 $ sampleExts ts s1 a
    F3 _ _ s0 s1 s2 -> sampleExts ts s0 $ sampleExts ts s1 $
                         sampleExts ts s2 a
    Append _ s0 -> sampleExts ts s0 a
    Drop _ s0 -> sampleExts ts s0 a
  where minSampPh :: Int
        minSampPh = 1

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

-- TRIGGERS -----------------------------

-- | To make customer C triggers.  Only for Spec Bool (others throw an
-- error).  
makeTrigger :: Outputs -> Trigger -> A.Atom () -> A.Atom ()
makeTrigger outputs trigger@(Trigger s fnName (vars,args)) r = 
  do r 
--     A.liftIO $ putStrLn ("len" ++ (show $ length vars)) 
     (A.exactPhase 2 $ A.atom (show trigger) $ 
        do A.cond (getOutput s)
           A.action (\ues -> fnName ++ "(" ++ unwords (intersperse "," ues) ++ ")")
              (reorder vars []
                (foldStreamableMaps 
                  (\v argSpec ls -> (v, A.ue $ getOutput argSpec):ls) args [])))
     where getOutput :: Streamable a => Spec a -> A.E a
           getOutput v = A.value (notVarErr v (\var -> getElem var outputs))
           reorder [] acc _ = reverse $ snd (unzip acc)
           reorder (v:vs) acc ls = 
               case lookup v ls of
                 Nothing -> error "Error in makeTrigger in Core.hs."
                 Just x  -> let n = (v,x)
                            in reorder vs (n:acc) ls
-- makeTrigger :: StreamableMaps Spec -> ProphArrs -> TmpSamples -> Indexes 
--             -> Trigger -> A.Atom () -> A.Atom ()
-- makeTrigger streams prophArrs tmpSamples outputIndexes 
--             trigger@(Trigger s fnName (vars,args)) r = 
--   do r 
--      A.liftIO $ putStrLn ("len" ++ (show $ length vars)) 
--      (A.exactPhase 0 $ A.atom (show trigger) $ 
--         do A.cond (nextSt streams prophArrs tmpSamples outputIndexes s 0)
--            A.action (\ues -> fnName ++ "(" ++ unwords (intersperse "," ues) ++ ")")
--               (reorder vars []
--                 (foldStreamableMaps 
--                   (\v argSpec ls -> (v,next argSpec):ls) args [])))
--      where next :: Streamable a => Spec a -> A.UE 
--            next a = A.ue $ nextSt streams prophArrs 
--                              tmpSamples outputIndexes a 0
--            reorder [] acc _ = reverse $ snd (unzip acc)
--            reorder (v:vs) acc ls = 
--                case lookup v ls of
--                  Nothing -> error "Error in makeTrigger in Core.hs."
--                  Just x  -> let n = (v,x)
--                             in reorder vs (n:acc) ls



-- bound min, max send phases
-- getOptimalPeriod :: StreamableMaps Spec -> StreamableMaps Send -> Period
-- getOptimalPeriod streams sends =
--   max (foldStreamableMaps getMaximumSamplingPhase streams 2)
-- --      (foldStreamableMaps getMaxSendPhase sends 0)
--   where
--     getMaximumSamplingPhase :: Var -> Spec a -> Period -> Period 
--     getMaximumSamplingPhase _ spec n =
--       case spec of
--         PVar _ _ -> n -- max (ph + 1) n 
--         PArr _ (_, Var _) -> max (ph + 2) n -- because this may depend on a
--                                             -- variable, and if that variable
--                                             -- has a prophecy array, it needs an
--                                             -- extra phase to update after the
--                                             -- index is taken.
--         PArr _ _ ph -> max (ph + 1) n
--         F _ _ s -> getMaximumSamplingPhase "" s n
--         F2 _ _ s0 s1 -> maximum [n,
--                 (getMaximumSamplingPhase "" s0 n),
--                 (getMaximumSamplingPhase "" s1 n)]
--         F3 _ _ s0 s1 s2 -> maximum [n,
--                 (getMaximumSamplingPhase "" s0 n), 
--                 (getMaximumSamplingPhase "" s1 n), 
--                 (getMaximumSamplingPhase "" s2 n)]
--         Drop _ s -> getMaximumSamplingPhase "" s n
--         Append _ s -> getMaximumSamplingPhase "" s n
--         _ -> n

--     -- getMaxSendPhase :: Var -> Send a -> Period -> Period
--     -- getMaxSendPhase _ (Send _ ph _ _) n = max (ph+1) n
