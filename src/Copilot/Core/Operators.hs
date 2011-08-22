--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Operators
  ( Op1 (..)
  , Op2 (..)
  , Op3 (..)
  , WrapOp1 (..)
  , WrapOp2 (..)
  , WrapOp3 (..)
  ) where

import Copilot.Core.Type (Type)
import Data.Bits

--------------------------------------------------------------------------------

-- | Unary operators.
class Op1 op where
  -- Boolean operators.
  not      :: op Bool Bool
  -- Numeric operators.
  abs   :: Num a => Type a -> op a a
  sign  :: Num a => Type a -> op a a
  -- Fractional operators.
  recip :: Fractional a => Type a -> op a a
  -- Floating operators.
  exp   :: Floating a => Type a -> op a a
  sqrt  :: Floating a => Type a -> op a a
  log   :: Floating a => Type a -> op a a
  sin   :: Floating a => Type a -> op a a
  tan   :: Floating a => Type a -> op a a
  cos   :: Floating a => Type a -> op a a
  asin  :: Floating a => Type a -> op a a
  atan  :: Floating a => Type a -> op a a
  acos  :: Floating a => Type a -> op a a
  sinh  :: Floating a => Type a -> op a a
  tanh  :: Floating a => Type a -> op a a
  cosh  :: Floating a => Type a -> op a a
  asinh :: Floating a => Type a -> op a a
  atanh :: Floating a => Type a -> op a a
  acosh :: Floating a => Type a -> op a a
  bwNot :: Bits     a => Type a -> op a a

--------------------------------------------------------------------------------

-- | Binary operators.
class Op2 op where
  -- Boolean operators.
  and      :: op Bool Bool Bool
  or       :: op Bool Bool Bool
  -- Numeric operators.
  add      :: Num a => Type a -> op a a a
  sub      :: Num a => Type a -> op a a a
  mul      :: Num a => Type a -> op a a a
  -- Integral operators.
  mod      :: Integral a => Type a -> op a a a
  div      :: Integral a => Type a -> op a a a
  -- Fractional operators.
  fdiv     :: Fractional a => Type a -> op a a a
  -- Floating operators.
  pow      :: Floating a => Type a -> op a a a
  logb     :: Floating a => Type a -> op a a a
  -- Equality operators.
  eq       :: Eq a => Type a -> op a a Bool
  ne       :: Eq a => Type a -> op a a Bool
  -- Relational operators.
  le       :: Ord a => Type a -> op a a Bool
  ge       :: Ord a => Type a -> op a a Bool
  lt       :: Ord a => Type a -> op a a Bool
  gt       :: Ord a => Type a -> op a a Bool
  -- Bitwise operators.
  bwAnd    :: Bits a => Type a -> op a a a
  bwOr     :: Bits a => Type a -> op a a a
  bwXor    :: Bits a => Type a -> op a a a
  bwShiftL :: ( Bits a, Integral b ) => Type a -> Type b -> op a b a
  bwShiftR :: ( Bits a, Integral b ) => Type a -> Type b -> op a b a

--------------------------------------------------------------------------------

-- | Ternary operators.
class Op3 op where
  -- Conditional operator:
  mux   :: Type a -> op Bool a a a

--------------------------------------------------------------------------------

-- Operator wrappers.

data WrapOp1 a b     = WrapOp1 { unWrapOp1 :: forall op . Op1 op => op a b }
data WrapOp2 a b c   = WrapOp2 { unWrapOp2 :: forall op . Op2 op => op a b c }
data WrapOp3 a b c d = WrapOp3 { unWrapOp3 :: forall op . Op3 op => op a b c d }

--------------------------------------------------------------------------------
