--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Expr
  ( Id
  , Name
  , Expr (..)
  , Op1 (..)
  , Op2 (..)
  , Op3 (..)
  ) where

import Copilot.Core.Type (Type)
import Data.Word (Word8)

--------------------------------------------------------------------------------

-- | A stream identifier.
type Id = Int

--------------------------------------------------------------------------------

-- | A name of a trigger, an external variable, or an external function.
type Name = String

--------------------------------------------------------------------------------

-- | The expression class.
class Expr e where
  -- | A constant.
  const
    :: Show a
    => Type a
    -> a
    -> e a
  -- | The temporal look-ahead operator.
  drop
    :: Type a
    -> Word8
    -> Id
    -> e a
  -- | An external variable.
  extern
    :: Type a
    -> Name
    -> e a
  -- | An unary operator.
  op1
    :: (forall op . Op1 op => op a b)
    -> e a -> e b
  -- | A binary operator.
  op2
    :: (forall op . Op2 op => op a b c)
    -> e a -> e b -> e c
  -- | A Ternary operator.
  op3
    :: (forall op . Op3 op => op a b c d)
    -> e a -> e b -> e c -> e d

--------------------------------------------------------------------------------

-- | Unary operators.
class Op1 op where
  -- Boolean operators.
  not  :: op Bool Bool
  -- Numeric operators.
  abs  :: Num a => Type a -> op a a
  sign :: Num a => Type a -> op a a

--------------------------------------------------------------------------------

-- | Binary operators.
class Op2 op where
  -- Boolean operators.
  and  :: op Bool Bool Bool
  or   :: op Bool Bool Bool
  -- Numeric operators.
  add  :: Num a => Type a -> op a a a
  sub  :: Num a => Type a -> op a a a
  mul  :: Num a => Type a -> op a a a
  -- Integral operators.
  mod  :: Integral a => Type a -> op a a a 
  div  :: Integral a => Type a -> op a a a 
  -- Equality operators.
  eq   :: Eq a => Type a -> op a a Bool
  ne   :: Eq a => Type a -> op a a Bool
  -- Relational operators.
  le   :: Ord a => Type a -> op a a Bool
  ge   :: Ord a => Type a -> op a a Bool
  lt   :: Ord a => Type a -> op a a Bool
  gt   :: Ord a => Type a -> op a a Bool

--------------------------------------------------------------------------------

-- | Ternary operators.
class Op3 op where
  -- Conditional operator:
  mux  :: Type a -> op Bool a a a

--------------------------------------------------------------------------------