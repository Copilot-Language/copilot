--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, Rank2Types #-}

module Copilot.Core.Operators
  ( Op1 (..)
  , Op2 (..)
  , Op3 (..)
  ) where

import Copilot.Core.Type (Type)
import Data.Bits

--------------------------------------------------------------------------------

-- Unary operators.
data Op1 a b where
  -- Boolean operators.
  Not      :: Op1 Bool Bool
  -- Numeric operators.
  Abs      :: Num a => Type a -> Op1 a a
  Sign     :: Num a => Type a -> Op1 a a
  -- Fractional operators.
  Recip    :: Fractional a => Type a -> Op1 a a
  -- Floating operators.
  Exp      :: Floating a => Type a -> Op1 a a
  Sqrt     :: Floating a => Type a -> Op1 a a
  Log      :: Floating a => Type a -> Op1 a a
  Sin      :: Floating a => Type a -> Op1 a a
  Tan      :: Floating a => Type a -> Op1 a a
  Cos      :: Floating a => Type a -> Op1 a a
  Asin     :: Floating a => Type a -> Op1 a a
  Atan     :: Floating a => Type a -> Op1 a a
  Acos     :: Floating a => Type a -> Op1 a a
  Sinh     :: Floating a => Type a -> Op1 a a
  Tanh     :: Floating a => Type a -> Op1 a a
  Cosh     :: Floating a => Type a -> Op1 a a
  Asinh    :: Floating a => Type a -> Op1 a a
  Atanh    :: Floating a => Type a -> Op1 a a
  Acosh    :: Floating a => Type a -> Op1 a a
  -- Bitwise operators.
  BwNot    :: Bits     a => Type a -> Op1 a a
  -- Casting operator.
  Cast     :: (Integral a, Num b) => Type a -> Type b -> Op1 a b

-- | Binary operators.
data Op2 a b c where
  -- Boolean operators.
  And      :: Op2 Bool Bool Bool
  Or       :: Op2 Bool Bool Bool
  -- Numeric operators.
  Add      :: Num a => Type a -> Op2 a a a
  Sub      :: Num a => Type a -> Op2 a a a
  Mul      :: Num a => Type a -> Op2 a a a
  -- Integral operators.
  Mod      :: Integral a => Type a -> Op2 a a a
  Div      :: Integral a => Type a -> Op2 a a a
  -- Fractional operators.
  Fdiv     :: Fractional a => Type a -> Op2 a a a
  -- Floating operators.
  Pow      :: Floating a => Type a -> Op2 a a a
  Logb     :: Floating a => Type a -> Op2 a a a
  -- Equality operators.
  Eq       :: Eq a => Type a -> Op2 a a Bool
  Ne       :: Eq a => Type a -> Op2 a a Bool
  -- Relational operators.
  Le       :: Ord a => Type a -> Op2 a a Bool
  Ge       :: Ord a => Type a -> Op2 a a Bool
  Lt       :: Ord a => Type a -> Op2 a a Bool
  Gt       :: Ord a => Type a -> Op2 a a Bool
  -- Bitwise operators.
  BwAnd    :: Bits a => Type a -> Op2 a a a
  BwOr     :: Bits a => Type a -> Op2 a a a
  BwXor    :: Bits a => Type a -> Op2 a a a
  BwShiftL :: ( Bits a, Integral b ) => Type a -> Type b -> Op2 a b a
  BwShiftR :: ( Bits a, Integral b ) => Type a -> Type b -> Op2 a b a

-- | Ternary operators.
data Op3 a b c d where
  -- Conditional operator:
  Mux   :: Type a -> Op3 Bool a a a
