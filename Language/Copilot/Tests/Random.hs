{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, GADTs, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Copilot.Tests.Random (randomStreams, Operator(..), Operators, fromOp) where

import Language.Copilot.Core
import Language.Copilot.Analyser

import qualified Language.Atom as A

import qualified Data.Map as M
import Prelude 
import System.Random
import Data.Int
import Data.Word
import Data.Maybe

---- Parameters of the random generation ---------------------------------------

maxDrop, maxSamplePhase :: Int
maxDrop = 4 -- The maximum value for i in Drop i _
maxSamplePhase = 8 -- The maximum value for ph in PVar _ _ ph

-- These determines the number of streams and of monitored variables.
-- Bools are drawn each time with the following weights, until a False is drawn
weightsContinueVar, weightsContinuePVar :: [(Bool, Int)]
weightsContinueVar = [(True, 3), (False, 1)]
weightsContinuePVar = [(True, 1), (False, 1)]

-- These determines the frequency of each atom type for the streams and the monitored variables
weightsVarTypes, weightsPVarTypes :: [(A.Type, Int)]
weightsVarTypes = 
    [(A.Bool, 5), (A.Word64, 3), (A.Int64, 3), (A.Float, 0), (A.Double, 4),
            (A.Int8, 1), (A.Int16, 1), (A.Int32, 1), (A.Word8, 1), (A.Word16, 1), (A.Word32, 1)]
weightsPVarTypes = 
    [(A.Bool, 5), (A.Word64, 3), (A.Int64, 3), (A.Float, 0), (A.Double, 4),
            (A.Int8, 1), (A.Int16, 1), (A.Int32, 1), (A.Word8, 1), (A.Word16, 1), (A.Word32, 1)]
            
-- These determines the frequency of each constructor in the random streams
-- 0 -> PVar
-- 1 -> Var
-- 2 -> Const
-- 3 -> F
-- 4 -> F2
-- 5 -> F3
-- 6 -> Append
-- 7 -> Drop
weightsAllSpecSet, weightsFunSpecSet, weightsDropSpecSet :: [(Int, Int)]
weightsAllSpecSet = [(0, 2),(1,2),(2,6),(3,1),(4,3),(5,1),(6,4),(7,1)]
weightsFunSpecSet = [(0, 2),(1,2),(2,6),(3,1),(4,3),(5,1),(7,1)]
weightsDropSpecSet = [(1,2),(2,6),(7,1)]

---- Tools ---------------------------------------------------------------------

data Operator a = 
    Operator (forall g . RandomGen g => (forall a' g'. (Streamable a', Random a', RandomGen g') => g' -> SpecSet -> (Spec a', g')) -> 
    g -> (Spec a, g))
type Operators = StreamableMaps Operator

fromOp :: Operator a -> 
    (forall g. RandomGen g => (forall a' g'. (Streamable a', Random a', RandomGen g') => g' -> SpecSet -> (Spec a', g')) -> 
    g -> (Spec a, g))
fromOp op =
    case op of
        Operator x -> x

foldRandomableMaps :: forall b c. 
    (forall a. (Streamable a, Random a) => Var -> c a -> b -> b) -> 
    StreamableMaps c -> b -> b
foldRandomableMaps f (SM bm i8m i16m i32m i64m w8m w16m w32m w64m fm dm) acc =
    let acc0 = M.foldWithKey f acc bm
        acc1 = M.foldWithKey f acc0 i8m        
        acc2 = M.foldWithKey f acc1 i16m
        acc3 = M.foldWithKey f acc2 i32m
        acc4 = M.foldWithKey f acc3 i64m
        acc5 = M.foldWithKey f acc4 w8m
        acc6 = M.foldWithKey f acc5 w16m
        acc7 = M.foldWithKey f acc6 w32m
        acc8 = M.foldWithKey f acc7 w64m
        acc9 = M.foldWithKey f acc8 fm      
        acc10 = M.foldWithKey f acc9 dm
    in acc10

randomWeighted :: (RandomGen g, Random a) => g -> [(a, Int)] -> (a, g)
randomWeighted g l =
    let l' = concatMap (\ (x, n) -> replicate n x) l
        len = length l'
        (i, g') = randomR (0, len - 1) g in
    (l' !! i, g')

data VName a = VName a
type Variables = StreamableMaps VName

---- Instances of Random -------------------------------------------------------

instance Random Int8 where
    random g = 
        let ((i::Int), g') = random g in
        (fromInteger $ toInteger i, g')
    randomR (lo, hi) g =
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')

instance Random Int16 where
    random g = 
        let ((i::Int), g') = random g in
        (fromInteger $ toInteger i, g')
    randomR (lo, hi) g =
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')

instance Random Int32 where
    random g = 
        let ((i::Int), g') = random g in
        (fromInteger $ toInteger i, g')
    randomR (lo, hi) g = 
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')

instance Random Int64 where
    random g = 
        let ((i0::Int32), g0) = random g
            ((i1::Int32), g1) = random g0 in
        (fromInteger (toInteger i0) + fromInteger (toInteger i1) * 2 ^ (32::Int), g1)
    randomR (lo, hi) g = -- TODO : generate on the whole range, and not only on a part of it
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')

instance Random Word8 where
    random g = 
        let ((i::Int), g') = random g in
        (fromInteger $ toInteger i, g')
    randomR (lo, hi) g =
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')

instance Random Word16 where
    random g = 
        let ((i::Int), g') = random g in
        (fromInteger $ toInteger i, g')
    randomR (lo, hi) g =
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')


instance Random Word32 where
    random g = 
        let ((i0::Word16), g0) = random g
            ((i1::Word16), g1) = random g0 in
        (fromInteger (toInteger i0) + fromInteger (toInteger i1) * 2 ^ (16::Int), g1)
    randomR (lo, hi) g = -- TODO : generate on the whole range, and not only on a part of it
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')


instance Random Word64 where
    random g = 
        let ((i0::Word32), g0) = random g
            ((i1::Word32), g1) = random g0 in
        (fromInteger (toInteger i0) + fromInteger (toInteger i1) * 2 ^ (32::Int), g1)
    randomR (lo, hi) g = -- TODO : generate on the whole range, and not only on a part of it
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')

instance Random a => Random [a] where
    random g =
        let (x, g0) = random g
            (b, g1) = random g0
            (l, g2) = 
                if b 
                    then random g1
                    else ([], g1) in
        (x:l, g2)
    -- Totally useless as lists are not ordered (could be, however)
    randomR (_, _) g = random g

instance Random A.Type where
    random g =
        let (n, g') = randomR (0::Int, 10) g in
        (toEnum n, g')
    randomR (t, t') g =
        let (n, g') = randomR (fromEnum t, fromEnum t') g in
        (toEnum n, g')

---- Generation of random streams ----------------------------------------------

randomStreams :: RandomGen g => Operators -> Operators -> Operators -> g -> (StreamableMaps Spec, Vars)
randomStreams opsF opsF2 opsF3 g =
    let (vs, g0) = addRandomVNames weightsContinueVar weightsVarTypes g emptySM
        (exts, g1) = addRandomVNames weightsContinuePVar weightsPVarTypes g0 emptySM
        (streams, g2) = foldRandomableMaps (addRandomSpec opsF opsF2 opsF3 vs exts) vs (emptySM, g1)
        (vars, g3) = foldRandomableMaps addRandomExternal exts (emptySM, g2) in
    if isNothing $ check streams
        then (streams, vars)
        else randomStreams opsF opsF2 opsF3 g3
        
addRandomVNames :: RandomGen g => [(Bool, Int)] -> [(A.Type, Int)] -> g -> Variables -> (Variables, g)
addRandomVNames wContinue wTypes g vs =
    let (b, g0) = randomWeighted g wContinue
        (t, g1) = randomWeighted g0 wTypes
        (v_int::Word64, g2) = random g1
        v = "v" ++ show v_int
        vs' = 
            case t of
                A.Bool -> updateSubMap (\ m -> M.insert v (VName (unit::Bool)) m) vs
                A.Int8 -> updateSubMap (\ m -> M.insert v (VName (unit::Int8)) m) vs
                A.Int16 -> updateSubMap (\ m -> M.insert v (VName (unit::Int16)) m) vs
                A.Int32 -> updateSubMap (\ m -> M.insert v (VName (unit::Int32)) m) vs
                A.Int64 -> updateSubMap (\ m -> M.insert v (VName (unit::Int64)) m) vs
                A.Word8 -> updateSubMap (\ m -> M.insert v (VName (unit::Word8)) m) vs
                A.Word16 -> updateSubMap (\ m -> M.insert v (VName (unit::Word16)) m) vs
                A.Word32 -> updateSubMap (\ m -> M.insert v (VName (unit::Word32)) m) vs
                A.Word64 -> updateSubMap (\ m -> M.insert v (VName (unit::Word64)) m) vs 
                A.Float -> updateSubMap (\ m -> M.insert v (VName (unit::Float)) m) vs
                A.Double -> updateSubMap (\ m -> M.insert v (VName (unit::Double)) m) vs             
    in
    if b 
        then addRandomVNames wContinue wTypes g2 vs'
        else (vs', g2)

addRandomSpec :: forall a g. (Streamable a, Random a, RandomGen g) => 
    Operators -> Operators -> Operators -> Variables -> Variables -> 
    Var -> VName a -> (StreamableMaps Spec, g) -> (StreamableMaps Spec, g)
addRandomSpec opsF opsF2 opsF3 vs exts v _ (streams, g) =
    let (spec::(Spec a), g') = randomSpec vs exts opsF opsF2 opsF3 g AllSpecSet in
    (updateSubMap (\m -> M.insert v spec m) streams, g')

randomSpec :: forall a g. (Streamable a, RandomGen g, Random a) => 
    Variables -> Variables -> Operators -> Operators -> Operators -> g -> SpecSet -> (Spec a, g)
randomSpec vs exts opsF opsF2 opsF3 g set =
    let weights = case set of
            AllSpecSet -> weightsAllSpecSet
            FunSpecSet -> weightsFunSpecSet
            DropSpecSet -> weightsDropSpecSet
        (n::Int, g0) = randomWeighted g weights in
    case n of
            0 -> -- PVar
                case getVar g0 exts of
                    (Just v, g1) -> 
                        let (ph, g2) = randomR (1, maxSamplePhase)  g1 in
                        (PVar (atomType (unit::a)) v ph, g2)
                    (Nothing, g1) -> randomSpec' g1 set
            1 -> -- Var
                case getVar g0 vs of
                    (Just v, g1) -> (Var v, g1)
                    (Nothing, g1) -> randomSpec' g1 set
            2 -> -- Const
                let (e, g1) = random g0 in
                (Const e, g1)
            3 -> -- F
                getOpStream opsF g0
            4 -> -- F2
                getOpStream opsF g0
            5 -> -- F3
                getOpStream opsF g0
            6 -> -- Append
                let (ls, g1) = random g0
                    (s', g2) = randomSpec' g1 set in
                (Append ls s', g2)
            7 -> -- Drop
                let (i, g1) = randomR (1::Int, maxDrop) g0
                    (s', g2) = randomSpec' g1 DropSpecSet in
                (Drop i s', g2)
            _ -> error "Impossible"
    where 
        randomSpec' :: forall a' g'. (Streamable a', RandomGen g', Random a') => g' -> SpecSet -> (Spec a', g')
        randomSpec' = randomSpec vs exts opsF opsF2 opsF3
        getOpStream :: Operators -> g -> (Spec a, g)
        getOpStream ops g0 =
            let m = getSubMap ops
                ks = M.keys m
                len = length ks
            in
            if len > 0
                then
                    let (i, g1) = randomR (0::Int, len - 1) g0
                        k = ks !! i in
                    case fromJust $ M.lookup k m of
                        Operator op -> op randomSpec' g1
                else randomSpec' g0 set
        getVar :: g -> Variables -> (Maybe Var, g)
        getVar g0 variables =
            let m :: M.Map Var (VName a)
                m = getSubMap variables
                ks = M.keys m
                len = length ks
            in
            if len > 0 
                then 
                    let (i, g1) = randomR (0::Int, len - 1) g0 in
                    (Just  (ks !! i), g1)
                else (Nothing, g0)
                
addRandomExternal :: forall a g. (Streamable a, Random a, RandomGen g) => 
    Var -> VName a -> (Vars, g) -> (Vars, g)
addRandomExternal v _ (vars, g) =
    let (vals::[a], g') = randomExternalValues g in
    (updateSubMap (\m -> M.insert v vals m) vars, g')

randomExternalValues :: (Streamable a, Random a, RandomGen g) => g -> ([a], g)
randomExternalValues g  =
    let (oldG, newG) = split g in
    (randoms oldG, newG)
