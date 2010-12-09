{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

-- | Sets of operators for Tests.Random.hs

module Language.Copilot.Language.RandomOps (
--  mkOp, mkOp2, mkOp3, mkOp2Coerce, mkOp2Ord, mkOp2Eq,
  opsF, opsF2, opsF3,
  module Language.Copilot.Tests.Random
                                           ) where  

import qualified Language.Atom as A

import Prelude (($), Float, Double, error, zip, asTypeOf)
import Data.Int
import Data.Word
import System.Random
import Data.Map as M

import Language.Copilot.Core
import Language.Copilot.Language
import Language.Copilot.Analyser
import Language.Copilot.Tests.Random

mkOp :: (Random arg1, Streamable arg1) =>
    (Spec arg1 -> Spec r) -> Operator r
mkOp op =
    Operator (\ rand g ->
            let (s0, g0) = rand g FunSpecSet in
            (op s0, g0)
        )

mkOp2 :: (Random arg1, Random arg2, Streamable arg1, Streamable arg2) =>
    (Spec arg1 -> Spec arg2 -> Spec r) -> Operator r
mkOp2 op =
    Operator (\ rand g ->
            let (s0, g0) = rand g FunSpecSet 
                (s1, g1) = rand g0 FunSpecSet in
            (op s0 s1, g1)
        )
        
mkOp3 :: (Random arg1, Random arg2, Random arg3, 
    Streamable arg1, Streamable arg2, Streamable arg3) =>
    (Spec arg1 -> Spec arg2 -> Spec arg3 -> Spec r) -> Operator r
mkOp3 op =
    Operator (\ rand g ->
            let (s0, g0) = rand g FunSpecSet
                (s1, g1) = rand g0 FunSpecSet
                (s2, g2) = rand g1 FunSpecSet in
            (op s0 s1 s2, g2)
        )

mkOp2Coerce :: (Random arg1, Random arg2, Streamable arg1, Streamable arg2) =>
    (Spec arg1 -> Spec arg2 -> Spec r) -> arg1 -> arg2 -> Operator r
mkOp2Coerce op c0 c1 =
    Operator (\ rand g ->
            let (s0, g0) = rand g FunSpecSet
                (s1, g1) = rand g0 FunSpecSet in
            (op (s0 `asTypeOf` (Const c0)) (s1 `asTypeOf` (Const c1)), g1)
        )

mkOp2Ord :: forall r. 
              (forall arg. (Random arg, A.OrdE arg, Streamable arg) 
              => (Spec arg -> Spec arg -> Spec r)) -> Operator r
mkOp2Ord op =
    let opI8, opI16, opI32, opI64, opW8, opW16, opW32, opW64, opF, opD :: 
            RandomGen g 
            => (forall a' g'. (Streamable a', Random a', RandomGen g') 
               => g' -> SpecSet -> (Spec a', g')) -> g -> (Spec r, g)
        opI8 = fromOp $ mkOp2Coerce op (unit::Int8) (unit::Int8)
        opI16 = fromOp $ mkOp2Coerce op (unit::Int16) (unit::Int16)
        opI32 = fromOp $ mkOp2Coerce op (unit::Int32) (unit::Int32)
        opI64 = fromOp $ mkOp2Coerce op (unit::Int64) (unit::Int64)
        opW8 = fromOp $ mkOp2Coerce op (unit::Word8) (unit::Word8)
        opW16 = fromOp $ mkOp2Coerce op (unit::Word16) (unit::Word16)
        opW32 = fromOp $ mkOp2Coerce op (unit::Word32) (unit::Word32)
        opW64 = fromOp $ mkOp2Coerce op (unit::Word64) (unit::Word64)
        opF = fromOp $ mkOp2Coerce op (unit::Float) (unit::Float)
        opD = fromOp $ mkOp2Coerce op (unit::Double) (unit::Double) in
    Operator (\ rand g ->
            let (t, g0) = randomR (A.Int8, A.Double) g in
            case t of
                A.Int8 -> opI8 rand g0
                A.Int16 -> opI16 rand g0
                A.Int32 -> opI32 rand g0
                A.Int64 -> opI64 rand g0
                A.Word8 -> opW8 rand g0
                A.Word16 -> opW16 rand g0
                A.Word32 -> opW32 rand g0
                A.Word64 -> opW64 rand g0
                A.Float -> opF rand g0
                A.Double -> opD rand g0
                _ -> error "Impossible"
        )

mkOp2Eq :: forall r. (forall arg. 
    (Random arg, A.EqE arg, Streamable arg) =>
    (Spec arg -> Spec arg -> Spec r)) 
    -> Operator r
mkOp2Eq op =
    let opB, opI8, opI16, opI32, opI64, opW8, opW16, opW32, opW64, opF, opD :: 
            RandomGen g => 
              (forall a' g'. (Streamable a', Random a', RandomGen g') => 
                g' -> SpecSet -> (Spec a', g')) -> g -> (Spec r, g)
        opB = fromOp $ mkOp2Coerce op (unit::Bool) (unit::Bool)
        opI8 = fromOp $ mkOp2Coerce op (unit::Int8) (unit::Int8)
        opI16 = fromOp $ mkOp2Coerce op (unit::Int16) (unit::Int16)
        opI32 = fromOp $ mkOp2Coerce op (unit::Int32) (unit::Int32)
        opI64 = fromOp $ mkOp2Coerce op (unit::Int64) (unit::Int64)
        opW8 = fromOp $ mkOp2Coerce op (unit::Word8) (unit::Word8)
        opW16 = fromOp $ mkOp2Coerce op (unit::Word16) (unit::Word16)
        opW32 = fromOp $ mkOp2Coerce op (unit::Word32) (unit::Word32)
        opW64 = fromOp $ mkOp2Coerce op (unit::Word64) (unit::Word64)
        opF = fromOp $ mkOp2Coerce op (unit::Float) (unit::Float)
        opD = fromOp $ mkOp2Coerce op (unit::Double) (unit::Double) in
    Operator (\ rand g ->
            let (t, g0) = random g in
            case t of
                A.Bool -> opB rand g0
                A.Int8 -> opI8 rand g0
                A.Int16 -> opI16 rand g0
                A.Int32 -> opI32 rand g0
                A.Int64 -> opI64 rand g0
                A.Word8 -> opW8 rand g0
                A.Word16 -> opW16 rand g0
                A.Word32 -> opW32 rand g0
                A.Word64 -> opW64 rand g0
                A.Float -> opF rand g0
                A.Double -> opD rand g0
        )

---- Definition of each operator

not_ :: Operator Bool
not_ = mkOp not    
    
(+$), (-$), (*$) :: (Streamable a, A.NumE a, Random a) => Operator a
(+$) = mkOp2 (+)
(-$) = mkOp2 (-)
(*$) = mkOp2 (*)

(/$) :: (Streamable a, A.NumE a, Fractional a, Random a) => Operator a
(/$) = mkOp2 (/)

(<$), (<=$), (>=$), (>$) :: Operator Bool
(<$) = mkOp2Ord (<)
(<=$) = mkOp2Ord (<=)
(>=$) = mkOp2Ord (>=)
(>$) = mkOp2Ord (>)

(==$), (/=$) :: Operator Bool
(==$) = mkOp2Eq (==)
(/=$) = mkOp2Eq (/=)

(||$), (&&$), (^$), (==>$) :: Operator Bool
(||$) = mkOp2 (||)
(&&$) = mkOp2 (&&)
(^$) = mkOp2 (^)
(==>$) = mkOp2 (==>)

mux_ :: (Streamable a, Random a) => Operator a
mux_ = mkOp3 mux

-- Packing of the operators in StreamableMaps

createMapFromElems :: [val] -> M.Map Var val
createMapFromElems vals =
    let ks = [[x] | x <- ['a'..]]
        l = zip ks vals in
    M.fromAscList l

-- | opsF, opsF2 and opsF3 are fed to Tests.Random.randomStreams.  They allows
-- the random generated streams to include lots of operators.  If you add a new
-- operator to Copilot, it would be nice to add it to one of those, that way it
-- could be used in the random streams used for testing.  opsF holds all the
-- operators of arity 1, opsF2 of arity 2 and opsF3 of arity3 They are
-- StreamableMaps, because operators are sorted based on their return type.
opsF, opsF2, opsF3 :: Operators
opsF = emptySM {bMap = createMapFromElems [not_]}

opsF2 = emptySM {
        bMap = createMapFromElems [(<$), (<=$), (>=$), (>$), (==$), (/=$), (||$), (&&$), (^$), (==>$)],
        i8Map = createMapFromElems [(+$), (-$), (*$)],
        i16Map = createMapFromElems [(+$), (-$), (*$)],
        i32Map = createMapFromElems [(+$), (-$), (*$)],
        i64Map = createMapFromElems [(+$), (-$), (*$)],
        w8Map = createMapFromElems [(+$), (-$), (*$)],
        w16Map = createMapFromElems [(+$), (-$), (*$)],
        w32Map = createMapFromElems [(+$), (-$), (*$)],
        w64Map = createMapFromElems [(+$), (-$), (*$)],
        fMap = createMapFromElems [(+$), (-$), (*$), (/$)],
        dMap = createMapFromElems [(+$), (-$), (*$), (/$)]
    }

opsF3 = emptySM {
        bMap = createMapFromElems [mux_],
        i8Map = createMapFromElems [mux_],
        i16Map = createMapFromElems [mux_],
        i32Map = createMapFromElems [mux_],
        i64Map = createMapFromElems [mux_],
        w8Map = createMapFromElems [mux_],
        w16Map = createMapFromElems [mux_],
        w32Map = createMapFromElems [mux_],
        w64Map = createMapFromElems [mux_],
        fMap = createMapFromElems [mux_],
        dMap = createMapFromElems [mux_]
    }
