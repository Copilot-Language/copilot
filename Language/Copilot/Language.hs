{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, TypeSynonymInstances #-}
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
        module Language.Copilot.Language.Sampling,
        -- -- * The next functions provide easier access to typed external variables.
        -- extB, extI8, extI16, extI32, extI64,
        -- extW8, extW16, extW32, extW64, extF, extD,
        -- -- * The next functions provide easier access to typed external arrays.
        -- extArrB, extArrI8, extArrI16, extArrI32, extArrI64,
        -- extArrW8, extArrW16, extArrW32, extArrW64, extArrF, extArrD,
        -- -- * Construct a function to sample
        -- fun,
        -- -- * Set of operators from which to choose during the generation of random streams
        -- opsF, opsF2, opsF3,
        -- * Constructs of the copilot language
        drop, (++), (.=), -- (..|), 
        -- * The next functions help typing the send operations
        -- Warning: there is no typechecking of that yet
        -- sendB, sendI8, sendI16, sendI32, sendI64,
        send, port, -- , sendW16, sendW32, sendW64, sendF, sendD
        -- * Triggers
        trigger, void, (<>), -- (<>>), 
        -- * Safe casting
        module Language.Copilot.Language.Casting,
--        cast,
        -- * Boolean stream constants
        const, true, false
    ) where

import qualified Language.Atom as A
import Data.Int
import Data.Word
import qualified Data.Map as M
import Prelude ( Bool(..), Num(..), Float, Double, String, ($)
               , Fractional(..), fromInteger, Show(..))
import qualified Prelude as P
import Control.Monad.Writer (tell)

import Language.Copilot.Core
import Language.Copilot.Language.Sampling
import Language.Copilot.Language.Casting
--import Language.Copilot.Language.RandomOps

---- Operators and functions ---------------------------------------------------

not :: Spec Bool -> Spec Bool
not = F P.not A.not_


-- | Beware : crash without any possible recovery if a division by 0 happens.
-- Same risk with mod. Use div0 and mod0 if unsure.
mod, div :: (Streamable a, A.IntegralE a) => Spec a -> Spec a -> Spec a
mod = F2 P.mod A.mod_
div = F2 P.mod A.div_

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
  notVarErr v (\v' -> tell $ LangElems 
                               (updateSubMap (M.insert v' s) emptySM) 
                               emptySM 
                               M.empty)

port :: Int -> Port
port = Port

-- | Takes a function @name@, a port @number@, and a Copilot variable @v@ and
-- constructs a call to the C function @name(x,y)@ where @x@ is the value of the
-- Copilot stream @v@ and @y@ is the port number.
send :: Streamable a => String -> Port -> Spec a -> Streams
send portName thePort s = 
  tell $ LangElems 
           emptySM 
           (updateSubMap (M.insert (show sending) sending) emptySM) 
           M.empty
  where sending = Send s thePort portName

class ArgCl a where 
  (<>) :: Streamable b => Spec b -> a -> Args

instance ArgCl Args where 
  s <> (vars,sm) = notVarErr s (\v -> (v:vars, updateSubMap (\m -> M.insert v s m) sm))

instance Streamable b => ArgCl (Spec b) where 
  s <> s' = (update s (update s' void))
    where update x (vars,sm) = 
            notVarErr x (\v -> (v:vars,updateSubMap (\m -> M.insert v x m) sm))

-- | No C arguments 
void :: Args
void = ([],emptySM)

-- -- | Turn a @Spec Var@ into an arguement for a C function.
-- (<>) :: Streamable a => Spec a -> Args -> Args
-- s <> (vars,sm) = notVarErr s (\v -> (v:vars, updateSubMap (\m -> M.insert v s m) sm))

-- -- | Turn a @Spec Var@ into an arguement for a C function.
-- (<>>) :: (Streamable a, Streamable b) => Spec a -> Spec b -> Args
-- s <>> s' = (update s (update s' void))
--   where update x (vars,sm) = 
--           notVarErr x (\v -> (v:vars,updateSubMap (\m -> M.insert v x m) sm))

-- | XXX document
trigger :: Spec Bool -> String -> Args -> Streams
trigger v fnName args =
  tell $ LangElems 
           emptySM 
           emptySM 
           (M.insert (show trig) trig M.empty)
  where trig = Trigger v fnName args

-- | Coerces a type that is 'Streamable' into a Copilot constant.
const :: Streamable a => a -> Spec a
const = Const

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
infixr 1 <>

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
