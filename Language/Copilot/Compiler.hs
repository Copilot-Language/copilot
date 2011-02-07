{-# LANGUAGE ScopedTypeVariables, Rank2Types, GADTs #-}

-- XXX Clean this up!

-- | Transform the copilot specification in an atom one, and then compile that one.
module Language.Copilot.Compiler
  (copilotToAtom, tmpSampleStr, tmpArrName, tmpVarName
  ) where

import Language.Copilot.Core

import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.Word (Word32)

import qualified Language.Atom as A

-- | Compiles an /Copilot/ specification to an /Atom/ one.
-- The period is given as a Maybe : if it is Nothing, an optimal period will be chosen.
copilotToAtom :: LangElems -> Maybe Period -> Name -> (Period, A.Atom ()) 
copilotToAtom (LangElems streams sends triggers) p cFileName = 
  (p', A.period p' $ do
    prophArrs <- mapStreamableMapsM initProphArr streams
    outputs <- mapStreamableMapsM initOutput streams
    updateIndexes <- foldStreamableMaps makeUpdateIndex prophArrs (return M.empty)
    outputIndexes <- foldStreamableMaps makeOutputIndex prophArrs (return M.empty)
    tmpSamples <- foldStreamableMaps 
                    (\_ -> initExtSamples streams outputs prophArrs outputIndexes) 
                    streams 
                    (return emptyTmpSamples)

    let nextStates = makeStates $ 
          mapStreamableMaps (nextSt streams prophArrs tmpSamples outputIndexes 0) 
            streams

    -- One atom rule for each stream
    foldStreamableMaps (makeRule p' nextStates outputs prophArrs
                           updateIndexes outputIndexes) 
      streams (return ())

    M.fold (makeTrigger outputs cFileName) (return ()) triggers
           
    foldStreamableMaps (makeSend outputs) sends (return ())

    -- Sampling of the external variables.  Remove redundancies.
    sequence_ $ snd . unzip $ nubBy (\x y -> fst x == fst y) $ 
      foldStreamableMaps (\_ -> sampleExts outputs tmpSamples cFileName) streams []
    )
  where p' = period p
--  where
--    optP = getOptimalPeriod streams sends


-- | For period n >= 5:
-- Phase 0: Sample external vars (if any).
-- Phase 1: state update.
-- Phase 2: Compute output variables.
-- Phase 3: Fire triggers (if any).
-- Phase 3: send values (if any).
-- Phase 4 up to n: update indexes.
period :: Maybe Int -> Int
period p = 
  case p of
    Nothing -> minPeriod
    Just i -> if i >= minPeriod
                then i 
                else error $ "Copilot error: the period is too short, " 
                       ++ "it should be at least " ++ show minPeriod ++ " ticks."
  where minPeriod :: Int
        minPeriod = 5

-- For the prophecy arrays
type ArrIndex = Word32
type ProphArrs = StreamableMaps BoundedArray
type Outputs = StreamableMaps A.V
type Indexes = M.Map Var (A.V ArrIndex)

-- External variables
data PhasedValueVar a = PhV (A.V a)

data BoundedArray a = B ArrIndex (Maybe (A.A a))

-- nextSt :: Streamable a => StreamableMaps Spec -> ProphArrs -> TmpSamples -> Indexes 
--        -> Spec a -> ArrIndex -> A.E a
-- nextSt streams prophArrs tmpSamples outputIndexes s index = 
--     case s of
--         PVar _ v  -> 
--           let PhV var = getElem (tmpVarName v) (tmpVars tmpSamples) in
--           A.value var
--         PArr _ (v, idx) -> 
--           let PhA var = e tmp (tmpArrs tmpSamples) 
--               tmp = tmpArrName v (show idx) 
--               e a b = case getMaybeElem a b of
--                         Nothing -> 
--                           error "Error in application of getElem in nextSt."
--                         Just x  -> x 
--           in A.value var
--         Var v -> nextStVar v streams prophArrs tmpSamples outputIndexes index
--         Const e -> A.Const e
--         F _ f s0 -> f $ next s0 index 
--         F2 _ f s0 s1 -> 
--             f (next s0 index) 
--               (next s1 index)
--         F3 _ f s0 s1 s2 -> 
--             f (next s0 index) 
--               (next s1 index) 
--               (next s2 index)
--         Append _ s0 -> next s0 index
--         Drop i s0 -> next s0 (fromInteger (toInteger i) + index)
--     where next :: Streamable b => Spec b -> ArrIndex -> A.E b
--           next = nextSt streams prophArrs tmpSamples outputIndexes

-- nextStVar v streams prophArrs tmpSamples outputIndexes index = 
--   let B initLen maybeArr = getElem v prophArrs in
--   -- This check is extremely important It means that if x at time n depends on y
--   -- at time n then x is obtained not by y, but by inlining the definition of y
--   -- so it increases the size of code (sadly), but is the only thing preventing
--   -- race conditions from occuring
--   if index < initLen
--     then getVar v initLen maybeArr 
--     else let s0 = getElem v streams in 
--          nextSt streams prophArrs tmpSamples outputIndexes s0 (index - initLen)
--   where getVar :: Streamable a => Var -> ArrIndex -> Maybe (A.A a) -> A.E a
--         getVar v initLen maybeArr =
--            let outputIndex = case M.lookup v outputIndexes of
--                                Nothing -> error "Error in function getVar."
--                                Just x -> x
--                arr = case maybeArr of
--                        Nothing -> error "Error in function getVar (maybeArr)."
--                        Just x -> x in 
--            arr A.!. ((A.Const index + A.VRef outputIndex) `A.mod_`  
--                        (A.Const (initLen + 1)))


-- | Takes the NextSts and walks over the tree building the Atom expression.  
makeStates :: StreamableMaps NextSt -> StreamableMaps A.E
makeStates trees = 
  mapStreamableMaps (\_ -> makeState) trees
  where 
    makeState :: (Streamable a) => NextSt a -> A.E a
    makeState tree = 
      case tree of 
        ExpLeaf s -> s
        F1Node f s0 -> f (makeState s0) 
        F2Node f s0 s1 -> f (makeState s0) (makeState s1)
        F3Node f s0 s1 s2 -> f (makeState s0) (makeState s1) (makeState s2)
        VarRefLeaf v -> makeState (getElem v trees)

-- | A tree datatype that holds Atom expressions from the 'nextSt' function
-- | until we've got all variable references collected up.  In particular, we
-- | have told function applications.
data NextSt a where
    -- Leaves
    ExpLeaf :: A.E a -> NextSt a
    VarRefLeaf :: Var -> NextSt a
    -- Parent nodes
    F1Node :: (Streamable a, Streamable b) 
           => (A.E b -> A.E a) -> NextSt b -> NextSt a
    F2Node :: (Streamable a, Streamable b, Streamable c) 
           => (A.E b -> A.E c -> A.E a) -> NextSt b -> NextSt c -> NextSt a 
    F3Node :: (Streamable a, Streamable b, Streamable c, Streamable d) 
           => (A.E b -> A.E c -> A.E d -> A.E a) 
              -> NextSt b -> NextSt c -> NextSt d -> NextSt a

-- | Compute the next state value.
nextSt :: Streamable a 
       => StreamableMaps Spec -> ProphArrs -> TmpSamples -> Indexes -> ArrIndex 
       -> Var -> Spec a -> NextSt a
nextSt streams prophArrs tmpSamples outputIndexes index _ s = 
    case s of
        PVar _ v  -> ExpLeaf $ 
          let PhV var = getElem (tmpVarName v) (tmpVars tmpSamples) in
          A.value var
        PArr _ (v, idx) -> ExpLeaf $
          let PhA var = e tmp (tmpArrs tmpSamples) 
              tmp = tmpArrName v (show idx) 
              e a b = case getMaybeElem a b of
                        Nothing -> 
                          error "Error in application of getElem in nextSt."
                        Just x  -> x 
          in A.value var
        Var v -> nextStVar v streams prophArrs tmpSamples outputIndexes index
        Const e -> ExpLeaf $ A.Const e
        F _ f s0 -> F1Node f (next s0 index)
        F2 _ f s0 s1 -> F2Node f (next s0 index) (next s1 index)
        F3 _ f s0 s1 s2 -> F3Node f (next s0 index) (next s1 index) (next s2 index)
        Append _ s0 -> next s0 index
        Drop i s0 -> next s0 (fromInteger (toInteger i) + index)
    where next :: Streamable b => Spec b -> ArrIndex -> NextSt b
          next s' ind = 
            nextSt streams prophArrs tmpSamples outputIndexes ind undefined s'

-- | Get the next state when one stream references another Copilot stream variable.
nextStVar :: Streamable a => Var -> StreamableMaps Spec -> ProphArrs 
          -> TmpSamples -> Indexes -> ArrIndex -> NextSt a
nextStVar v streams prophArrs tmpSamples outputIndexes index = 
  let B initLen maybeArr = getElem v prophArrs in
  -- This check is extremely important. It means that if x at time n depends on y
  -- at time n then x is obtained not by y, but by inlining the definition of y
  -- so it increases the size of code (sadly), but is the only thing preventing
  -- race conditions from occuring.
  if index < initLen
    then getVar v initLen maybeArr 
    else let s0 = getElem v streams in 
         -- XXX This should be generalized to handle arbitrary index values.
         -- For now, as a test, we'll just use the cahced nextState value if
         -- it's like computing that stream's nextState from scratch.
         let newIndex = index - initLen in
         if newIndex == 0 then VarRefLeaf v
           else nextSt streams prophArrs tmpSamples outputIndexes 
                  newIndex undefined s0
  where getVar :: Streamable a => Var -> ArrIndex -> Maybe (A.A a) -> NextSt a
        getVar v initLen maybeArr = ExpLeaf $
           let outputIndex = case M.lookup v outputIndexes of
                               Nothing -> error "Error in function getVar."
                               Just x -> x
               arr = case maybeArr of
                       Nothing -> error "Error in function getVar (maybeArr)."
                       Just x -> x in 
           arr A.!. ((A.Const index + A.VRef outputIndex) `A.mod_`  
                       (A.Const (initLen + 1)))

initProphArr :: forall a. Streamable a 
             => Var -> Spec a -> A.Atom (BoundedArray a)
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
tmpVarName v = show v

tmpArrName :: Ext -> String -> Var
tmpArrName v idx = (tmpVarName v) ++ "_" ++ normalizeVar idx

initOutput :: forall a. Streamable a => Var -> Spec a -> A.Atom (A.V a)
initOutput v _ = do
  atomConstructor (normalizeVar v) (unit::a)

tmpSampleStr :: String
tmpSampleStr = "tmpSampleVal__"

initExtSamples :: forall a. Streamable a 
               => StreamableMaps Spec -> Outputs -> ProphArrs -> Indexes -> Spec a 
                  -> A.Atom TmpSamples -> A.Atom TmpSamples
initExtSamples streams outputs prophArrs outputIndexes s tmpSamples = do
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
            do  ts <- tmpSamples
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
            do  ts <- tmpSamples
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
                                   Var v   -> PhIdx $ A.value (getElem v outputs)
                                   _    -> error "Unexpected Spec in initExtSamples."
                         let m' = M.insert arr' (PhA val) (getSubMap arrts)
                         let m'' = M.insert arr' i (getSubMap idxts)
                         return $ ts { tmpArrs = updateSubMap (\_ -> m') arrts
                                     , tmpIdxs = updateSubMap (\_ -> m'') idxts
                                     }
                  Just _ -> return ts
    where initExtSamples' :: Streamable b
                          => Spec b -> A.Atom TmpSamples -> A.Atom TmpSamples
          initExtSamples' = initExtSamples streams outputs prophArrs outputIndexes

-- | For each stream, make an index into its array of values telling you where
-- the next value in the array to update is.
makeUpdateIndex :: Var -> BoundedArray a -> A.Atom Indexes -> A.Atom Indexes
makeUpdateIndex v (B n arr) indexes =
    case arr of
        Nothing -> indexes
        Just _ ->  
            do  mindexes <- indexes
                index <- atomConstructor ("updateIndex__" ++ normalizeVar v) n
                return $ M.insert v index mindexes

-- | For each stream, make an index into its array of values telling you where
-- its current output value is.
makeOutputIndex :: Var -> BoundedArray a -> A.Atom Indexes -> A.Atom Indexes
makeOutputIndex v (B _ arr) indexes =
    case arr of
        Nothing -> indexes
        Just _ ->  
            do  mindexes <- indexes
                index <- atomConstructor ("outputIndex__" ++ normalizeVar v) 0
                return $ M.insert v index mindexes

-- makeRule :: forall a. Streamable a => 
--     Period -> StreamableMaps Spec -> Outputs -> ProphArrs -> TmpSamples -> 
--     Indexes -> Indexes -> Var -> Spec a -> A.Atom () -> A.Atom ()
-- makeRule p streams outputs prophArrs tmpSamples updateIndexes outputIndexes v s r = do
--     r 
--     let B n maybeArr = getElem v prophArrs::BoundedArray a
--     case maybeArr of
--         Nothing ->
--             -- Fusing together the update and the output if the prophecy array doesn't exist 
--             -- (ie if it would only have hold the output value)
--             A.exactPhase 2 $ A.atom ("updateOutput__" ++ normalizeVar v) $ do
--                 ((getElem v outputs)::(A.V a)) A.<== nextSt'

--         Just arr -> do
--             let updateIndex = fromJust $ M.lookup v updateIndexes
--                 outputIndex = fromJust $ M.lookup v outputIndexes

--             A.exactPhase 1 $ A.atom ("update__" ++ normalizeVar v) $ do
--                 arr A.! (A.VRef updateIndex) A.<== nextSt'
            
--             A.exactPhase 2 $ A.atom ("output__" ++ normalizeVar v) $ do
--                 ((getElem v outputs)::(A.V a)) A.<== arr A.!. (A.VRef outputIndex)
--                 outputIndex A.<==          (A.VRef outputIndex + A.Const 1) 
--                                   `A.mod_` A.Const (n + 1)
            
--             A.phase 4
--               $ A.atom ("incrUpdateIndex__" ++ normalizeVar v) $ do
--                 updateIndex A.<==          (A.VRef updateIndex + A.Const 1) 
--                                   `A.mod_` A.Const (n + 1)

--        where nextSt' = nextSt streams prophArrs tmpSamples outputIndexes s 0
makeRule :: forall a. Streamable a => 
    Period -> StreamableMaps A.E -> Outputs -> ProphArrs
    -> Indexes -> Indexes -> Var -> Spec a -> A.Atom () -> A.Atom ()
makeRule p exps outputs prophArrs updateIndexes outputIndexes v s r = do
    r 
    let B n maybeArr = getElem v prophArrs::BoundedArray a
    case maybeArr of
        Nothing ->
            -- Fusing together the update and the output if the prophecy array doesn't exist 
            -- (ie if it would only have hold the output value)
            A.exactPhase 2 $ A.atom ("updateOutput__" ++ normalizeVar v) $ do
                ((getElem v outputs)::(A.V a)) A.<== getElem v exps

        Just arr -> do
            let updateIndex = fromJust $ M.lookup v updateIndexes
                outputIndex = fromJust $ M.lookup v outputIndexes

            A.exactPhase 1 $ A.atom ("update__" ++ normalizeVar v) $ do
                arr A.! (A.VRef updateIndex) A.<== getElem v exps
            
            A.exactPhase 2 $ A.atom ("output__" ++ normalizeVar v) $ do
                ((getElem v outputs)::(A.V a)) A.<== arr A.!. (A.VRef outputIndex)
                outputIndex A.<==          (A.VRef outputIndex + A.Const 1) 
                                  `A.mod_` A.Const (n + 1)
            
            A.phase 4
              $ A.atom ("incrUpdateIndex__" ++ normalizeVar v) $ do
                updateIndex A.<==          (A.VRef updateIndex + A.Const 1) 
                                  `A.mod_` A.Const (n + 1)
             
makeSend :: forall a. Streamable a 
         => Outputs -> String -> Send a -> A.Atom () -> A.Atom ()
makeSend outputs name (Send v port portName) r = do
        r 
        A.exactPhase 3 $ A.atom ("__send_" ++ name) $
            mkSend (A.value $ getElem (getMaybeVar v) outputs :: A.E a) 
                   port 
                   portName

-- | Sending data over ports.
mkSend :: (Streamable a) => A.E a -> Port -> String -> A.Atom ()
mkSend e (Port port) portName =
  A.action (\ueStr -> portName ++ "(" ++ head ueStr ++ "," ++ show port ++ ")") 
           [A.ue e]

sampleStr :: String
sampleStr = "sample__"

-- What we really should be doing is just folding over the TmpSamples, since
-- that data should contain all the info we need to construct external variable
-- and external array samples.  However, there is the issue that for array
-- samples, the type of the index may differ from the type of the array, and
-- having the spec available provides typing coercion.  We could fold over the
-- TmpSamples, passing streams in, and extract the appropriate Spec a.
sampleExts :: forall a. Streamable a 
           => Outputs -> TmpSamples -> Name -> Spec a 
              -> [(Var, A.Atom ())] -> [(Var, A.Atom ())]
sampleExts outputs ts cFileName s a = do
  case s of
    Var _ -> a
    Const _ -> a
    PVar _ v -> 
     let v' = tmpVarName v
--         PhV var = getElem v' (tmpVars ts) :: PhasedValueVar a in
         PhV var = case getMaybeElem v' (tmpVars ts) :: Maybe (PhasedValueVar a) of 
                     Nothing ->  error $ "Copilot error: variable " ++ v' 
                                   ++ " was not defined!."
                     Just (PhV var') -> PhV var' in
     (v', A.exactPhase 0 $ 
            A.atom (sampleStr ++ normalizeVar v') $ 
               var A.<== (A.value $ externalAtomConstructor $ getSampleFuncVar v)
     ) : a
    PArr _ (arr, idx) -> 
         let arr' = tmpArrName arr (show idx)
             PhIdx i = getIdx arr' idx (tmpIdxs ts)
             PhA arrV = 
               case getMaybeElem arr' (tmpArrs ts) :: Maybe (PhasedValueArr a) of
                 Nothing -> error "Error in fucntion sampleExts."
                 Just x -> x in
         (arr', A.exactPhase 0 $ 
            A.atom (sampleStr ++ normalizeVar arr') $ 
               arrV A.<== A.array' (getSampleFuncVar arr)
                                   (atomType (unit::a)) A.!. i
            ) : a
    F _ _ s0 -> sampleExts' s0 a
    F2 _ _ s0 s1 -> sampleExts' s0 $ sampleExts' s1 a
    F3 _ _ s0 s1 s2 -> sampleExts' s0 $ sampleExts' s1 $
                         sampleExts' s2 a
    Append _ s0 -> sampleExts' s0 a
    Drop _ s0 -> sampleExts' s0 a
  where -- minSampPh :: Int
        --minSampPh = 2
        sampleExts' s' a' = sampleExts outputs ts cFileName s' a'
        getSampleFuncVar v = case v of
                               ExtV extV -> extV
                               -- XXX A bit of a hack.  Atom should be changed to allow
                               -- "out-of-Atom" assignments.  But this works for now.
                               Fun nm args -> funcShow cFileName nm args


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
makeTrigger :: Outputs -> Name -> Trigger -> A.Atom () -> A.Atom ()
makeTrigger outputs cFileName trigger@(Trigger s fnName args) r = 
  do r 
     (A.exactPhase 3 $ A.atom (show trigger) $ 
        do A.cond (getOutput outputs s)
           fnCall cFileName fnName args)

-- | Building an external function call in Atom.
fnCall :: Name -> String -> Args -> A.Atom ()
fnCall cFileName fnName args = 
  A.action (\_ -> funcShow cFileName fnName args) []

getOutput :: Streamable a => Outputs -> Spec a -> A.E a
getOutput outputs s = 
  case s of
    (Var v) -> A.value 
      (case getMaybeElem v outputs of
         Nothing -> error $ "Copilot error in trigger specification: variable " 
                      ++ v ++ " was not defined!."
         Just v' -> v')
    (Const c) -> A.Const c
    _ -> error "Impossible error in getOutput in Compiler.hs."
