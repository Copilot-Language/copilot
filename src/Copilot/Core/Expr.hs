--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs, ExistentialQuantification #-}

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
import Data.Word (Word16)

--------------------------------------------------------------------------------

-- | A stream identifier.
type Id = Int

--------------------------------------------------------------------------------

-- | A name of a trigger, an external variable, or an external function.
type Name = String

--------------------------------------------------------------------------------

-- | An index for the drop operator.
type DropIdx = Word16

--------------------------------------------------------------------------------

-- | A unique tag for external arrays/function calls.
type Tag = Int

--------------------------------------------------------------------------------

data Expr a where
  Const        :: Type a -> a -> Expr a
  Drop         :: Type a -> DropIdx -> Id -> Expr a
  Local        :: Type a -> Type b -> Name -> Expr a -> Expr b -> Expr b
  Var          :: Type a -> Name -> Expr a 
  ExternVar    :: Type a -> Name -> Maybe [a] -> Expr a 
  ExternFun    :: Type a -> Name -> [UExpr] -> Maybe (Expr a) 
               -> Maybe Tag -> Expr a
  ExternArray  :: Integral a => Type a -> Type b -> Name -> Int -> Expr a
               -> Maybe [[b]] -> Maybe Tag -> Expr b 
  ExternStruct :: Name -> [UExpr] -> Stream [UExpr]
  Op1          :: Op1 a b -> Expr a -> Expr b 
  Op2          :: Op2 a b c -> Expr a -> Expr b -> Expr c
  Op3          :: Op3 a b c d -> Expr a -> Expr b -> Expr c -> Expr d

--------------------------------------------------------------------------------

-- | A untyped expression (no phantom type).
data UExpr = forall a. UExpr
  { uExprType :: Type a
  , uExprExpr :: Expr a }
