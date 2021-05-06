--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs, ExistentialQuantification #-}

-- | Internal representation of Copilot stream expressions.
module Copilot.Core.Expr
  ( Id
  , Name
  , Expr (..)
  , UExpr (..)
  , DropIdx
  , Tag
  ) where

import Copilot.Core.Operators (Op1, Op2, Op3)
import Copilot.Core.Type (Type)
import Data.Word (Word32)

import Data.Typeable (Typeable)

--------------------------------------------------------------------------------

-- | A stream identifier.
type Id = Int

--------------------------------------------------------------------------------

-- | A name of a trigger, an external variable, or an external function.
type Name = String

--------------------------------------------------------------------------------

-- | An index for the drop operator.
type DropIdx = Word32

--------------------------------------------------------------------------------

-- | A unique tag for external arrays/function calls.
type Tag = Int

--------------------------------------------------------------------------------

-- | Internal representation of Copilot stream expressions.
--
-- The Core representation mimics the high-level Copilot stream, but the Core
-- representation contains information about the types of elements in the
-- stream.
data Expr a where
  Const        :: Typeable a => Type a -> a -> Expr a
  Drop         :: Typeable a => Type a -> DropIdx -> Id -> Expr a
  Local        :: Typeable a => Type a -> Type b -> Name -> Expr a -> Expr b -> Expr b
  Var          :: Typeable a => Type a -> Name -> Expr a
  ExternVar    :: Typeable a => Type a -> Name -> Maybe [a] -> Expr a
  Op1          :: Typeable a => Op1 a b -> Expr a -> Expr b
  Op2          :: (Typeable a, Typeable b) => Op2 a b c -> Expr a -> Expr b -> Expr c
  Op3          :: (Typeable a, Typeable b, Typeable c) => Op3 a b c d -> Expr a -> Expr b -> Expr c -> Expr d
  Label        :: Typeable a => Type a -> String -> Expr a -> Expr a

--------------------------------------------------------------------------------

-- | A untyped expression that carries the information about the type of the
-- expression as a value, as opposed to exposing it at type level (using an
-- existential).
data UExpr = forall a. Typeable a => UExpr
  { uExprType :: Type a
  , uExprExpr :: Expr a }
