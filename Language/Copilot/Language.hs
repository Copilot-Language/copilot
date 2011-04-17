{-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Describes the language /Copilot/.
--
-- If you wish to add a new operator, the only modification needed is adding it
-- in this module.  But if you want it to be used in the random generated
-- streams, add it to either @'opsF'@, @'opsF2'@ or @'opsF3'@
module Language.Copilot.Language (
        -- * Operators and functions
        mod, div, mod0, div0,
        (<), (<=), (==), (/=), (>=), (>),
        not, (||), (&&), (^), (==>),
        -- * Boolean constants
        Bool(..),
        -- * Arithmetic operators (derived)
        Num(..),
        -- * Division
        Fractional((/)),
        mux, 
        -- * Copilot variable declarations.
        var, varB, varI8, varI16, varI32, varI64,
        varW8, varW16, varW32, varW64, varF, varD,
        -- * Copilot constant declarations.  For the most part, these are
        -- unnecessary, as constants are automatically lifted in into the *
        -- Copilot types.  They are useful though for specifying triggers and *
        -- function samplings.
        const, constI8, constI16, constI32, constI64,
        constW8, constW16, constW32, constW64, constF, constD,
        -- * Boolean stream constants
        true, false,
        module Language.Copilot.Language.Sampling,
        -- * Constructs of the copilot language
        drop, (++), (.=), -- (..|), 
        -- * Triggers
        module Language.Copilot.Language.FunctionCalls,
        -- * Safe casting
        module Language.Copilot.Language.Casting,
        notConstVarErr,
        A.EqE
    ) where

import qualified Language.Atom as A
import Data.Int
import Data.Word
import Prelude ( Bool(..), Num(..), Float, Double, ($), error
               , Fractional(..), fromInteger, Show(..))
import qualified Prelude as P
import Control.Monad.Writer (tell)
import qualified Data.Map as M

import Language.Copilot.Core
import Language.Copilot.Language.Sampling
import Language.Copilot.Language.Casting
import Language.Copilot.Language.FunctionCalls
--import Language.Copilot.Language.RandomOps

---- Operators and functions ---------------------------------------------------

not :: Spec Bool -> Spec Bool
not = F P.not A.not_


-- | Beware : crash without any possible recovery if a division by 0 happens.
-- Same risk with mod. Use div0 and mod0 if unsure.
mod, div :: (Streamable a, A.IntegralE a) => Spec a -> Spec a -> Spec a
mod = F2 P.mod A.mod_
div = F2 P.div A.div_

-- | As mod and div, except that if the division would be by 0, the first
-- argument is used as a default.
mod0, div0 :: (Streamable a, A.IntegralE a) => a -> Spec a -> Spec a -> Spec a
mod0 d = F2 (\ x0 x1 -> if x1 P.== 0 then x0 `P.div` d 
                          else x0 `P.div` x1) 
            (\ e0 e1 -> A.mod0_ e0 e1 d)
div0 d = F2 (\ x0 x1 -> if x1 P.== 0 then x0 `P.mod` d 
                          else x0 `P.mod` x1) 
            (\ e0 e1 -> A.div0_ e0 e1 d)

  
-- class (Streamable a, A.OrdE a) => SpecOrd a where
--   (<)

(<), (<=), (>=), (>) :: (Streamable a, A.OrdE a) => Spec a -> Spec a -> Spec Bool
(<) = F2 (P.<) (A.<.)
(<=) = F2 (P.<=) (A.<=.)
(>=) = F2 (P.>=) (A.>=.)
(>) = F2 (P.>) (A.>.)

(==), (/=) :: (Streamable a, A.EqE a) => Spec a -> Spec a -> Spec Bool
(==) = F2 (P.==) (A.==.)
(/=) = F2 (P./=) (A./=.)

(||), (&&), (^), (==>) :: Spec Bool -> Spec Bool -> Spec Bool
(||) = F2 (P.||) (A.||.)
(&&) = F2 (P.&&) (A.&&.)
(^) = F2 
    (\ x y -> (x P.&& P.not y) P.|| (y P.&& P.not x)) 
    (\ x y -> (x A.&&. A.not_ y) A.||. (y A.&&. A.not_ x))
(==>) = F2 (\ x y -> y P.|| P.not x) A.imply

-- | Beware : both sides are executed, even if the result of one is later discarded
mux :: (Streamable a) => Spec Bool -> Spec a -> Spec a -> Spec a
mux = F3 (\ b x y -> if b then x else y) A.mux

infix 5 ==, /=, <, <=, >=, >
infixr 4 ||, &&, ^, ==>


---- Constructs of the language ------------------------------------------------


-- If a generic 'var' declaration is insufficient for the type-checker to
-- determine the type, a monomorphic var operator can be used.

-- | Useful for writing libraries.
var :: Streamable a => Var -> Spec a
var = Var

varB :: Var -> Spec Bool
varB = Var
varI8 :: Var -> Spec Int8
varI8 = Var
varI16 :: Var -> Spec Int16
varI16 = Var
varI32 :: Var -> Spec Int32
varI32 = Var
varI64 :: Var -> Spec Int64
varI64 = Var
varW8 :: Var -> Spec Word8
varW8 = Var 
varW16 :: Var -> Spec Word16
varW16 = Var
varW32 :: Var -> Spec Word32
varW32 = Var
varW64 :: Var -> Spec Word64
varW64 = Var
varF :: Var -> Spec Float
varF = Var
varD :: Var -> Spec Double
varD = Var

-- | Define a stream variable.
(.=) :: Streamable a => Spec a -> Spec a -> Streams
v .= s = 
  case v of
    (Var v') -> tell $ LangElems (updateSubMap (M.insert v' s) emptySM) 
                                 M.empty
    _ -> error $ "Given spec " P.++ show v 
                   P.++ " but expected a variable in a Copilot definition (.=)."

-- | Coerces a type that is 'Streamable' into a Copilot constant.
const :: Streamable a => a -> Spec a
const = Const

constI8 :: Int8 -> Spec Int8
constI8 = Const
constI16 :: Int16 -> Spec Int16
constI16 = Const
constI32 :: Int32 -> Spec Int32
constI32 = Const
constI64 :: Int64 -> Spec Int64
constI64 = Const
constW8 :: Word8 -> Spec Word8
constW8 = Const 
constW16 :: Word16 -> Spec Word16
constW16 = Const
constW32 :: Word32 -> Spec Word32
constW32 = Const
constW64 :: Word64 -> Spec Word64
constW64 = Const
constF :: Float -> Spec Float
constF = Const
constD :: Double -> Spec Double
constD = Const

true, false :: Spec Bool
true = Const True
false = Const False

-- | Drop @i@ elements from a stream.
drop :: Streamable a => Int -> Spec a -> Spec a
drop i s = Drop i s

-- | Just a trivial wrapper over the @'Append'@ constructor
(++) :: Streamable a => [a] -> Spec a -> Spec a
ls ++ s = Append ls s


infixr 3 ++
infixr 2 .=

---- Optimisation rules --------------------------------------------------------

{-# RULES
"Copilot.Language Plus0R" forall s. (P.+) s (Const 0) = s
"Copilot.Language Plus0L" forall s. (P.+) (Const 0) s = s
"Copilot.Language Minus0R" forall s. (P.-) s (Const 0) = s
"Copilot.Language Minus0L" forall s. (P.-) (Const 0) s = s
"Copilot.Language Times1R" forall s. (P.*) s (Const 1) = s
"Copilot.Language Times1L" forall s. (P.*) (Const 1) s = s
"Copilot.Language Times0R" forall s. (P.*) s (Const 0) = Const 0
"Copilot.Language Times0L" forall s. (P.*) (Const 0) s = Const 0
"Copilot.Language FracBy0" forall s. (P./) s (Const 0.0) = P.error "division by zero !" 
"Copilot.Language FracBy1" forall s. (P./) s (Const 1.0) = s 
"Copilot.Language Frac0" forall s. (P./) (Const 0.0) s = (Const 0.0)
"Copilot.Language OrFR" forall s. (||) s (Const False) = s
"Copilot.Language OrFL" forall s. (||) (Const False) s = s
"Copilot.Language OrTR" forall s. (||) s (Const True) = Const True
"Copilot.Language OrTL" forall s. (||) (Const True) s = Const True
"Copilot.Language AndFR" forall s. (&&) s (Const False) = Const False
"Copilot.Language AndFL" forall s. (&&) (Const False) s = Const False
"Copilot.Language AndTR" forall s. (&&) s (Const True) = s
"Copilot.Language AndTL" forall s. (&&) (Const True) s = s
"Copilot.Language ImpliesFL" forall s. (==>) (Const False) s = Const True
"Copilot.Language NotF" not (Const False) = Const True
"Copilot.Language NotT" not (Const True) = Const False
"Copilot.Language MuxF" forall s0 s1. mux (Const False) s0 s1 = s1
"Copilot.Language MuxT" forall s0 s1. mux (Const True) s0 s1 = s0
"Copilot.Language ImpliesDef" forall s0 s1. (||) s1 (not s0) = s0 ==> s1
      #-}

-- "Copilot.Core CastLift" forall s i. drop i (cast s) = cast (drop i s)
