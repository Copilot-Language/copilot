-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE ExistentialQuantification #-}
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

-- | A stream identifier.
type Id = Int

-- | A name of a trigger, an external variable, or an external function.
type Name = String

-- | The expression class.
class Expr η where
  -- | A constant.
  const
    :: Show α
    => Type α
    -> α
    -> η α
  -- | The temporal look-ahead operator.
  drop
    :: Type α
    -> Word8
    -> Id
    -> η α
  -- | An external variable.
  extern
    :: Type α
    -> Name
    -> η α
  -- | An unary operator.
  op1
    :: (forall θ . Op1 θ => θ α β)
    -> η α -> η β
  -- | A binary operator.
  op2
    :: (forall θ . Op2 θ => θ α β γ)
    -> η α -> η β -> η γ
  -- | A Ternary operator.
  op3
    :: (forall θ . Op3 θ => θ α β γ δ)
    -> η α -> η β -> η γ -> η δ

-- | Unary operators.
class Op1 θ where
  -- Boolean operators.
  not  :: θ Bool Bool
  -- Numeric operators.
  abs  :: Num α => Type α -> θ α α
  sign :: Num α => Type α -> θ α α

-- | Binary operators.
class Op2 θ where
  -- Boolean operators.
  and  :: θ Bool Bool Bool
  or   :: θ Bool Bool Bool
  -- Numeric operators.
  add  :: Num α => Type α -> θ α α α
  sub  :: Num α => Type α -> θ α α α
  mul  :: Num α => Type α -> θ α α α
  -- Integral operators.
  mod  :: Integral α => Type α -> θ α α α 
  div  :: Integral α => Type α -> θ α α α 
  -- Equality operators.
  eq   :: Eq α => Type α -> θ α α Bool
  ne   :: Eq α => Type α -> θ α α Bool
  -- Relational operators.
  le   :: Ord α => Type α -> θ α α Bool
  ge   :: Ord α => Type α -> θ α α Bool
  lt   :: Ord α => Type α -> θ α α Bool
  gt   :: Ord α => Type α -> θ α α Bool

-- | Ternary operators.
class Op3 θ where
  -- Conditional operator:
  mux  :: Type α -> θ Bool α α α

{-
class
  ( Show α
  , Typed α
  , Uninitializable α
  ) => Streamable α

instance Streamable Bool
instance Streamable Int8
instance Streamable Int16
instance Streamable Int32
instance Streamable Int64
instance Streamable Word8
instance Streamable Word16
instance Streamable Word32
instance Streamable Word64
-}
