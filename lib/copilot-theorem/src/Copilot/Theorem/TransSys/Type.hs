--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# LANGUAGE Safe #-}

-- | Types suported by the modular transition systems.
module Copilot.Theorem.TransSys.Type
  ( Type (..)
  , U (..)
  , U2 (..)
  ) where

import Copilot.Core.Type.Equality

--------------------------------------------------------------------------------

-- | A type at both value and type level.
--
-- Real numbers are mapped to 'Double's.
data Type a where
  Bool    :: Type Bool
  Integer :: Type Integer
  Real    :: Type Double

-- | Proofs of type equality.
instance EqualType Type where
  Bool    =~= Bool     = Just Refl
  Integer =~= Integer  = Just Refl
  Real    =~= Real     = Just Refl
  _       =~= _        = Nothing

--------------------------------------------------------------------------------

-- | Unknown types.
--
-- For instance, 'U Expr' is the type of an expression of unknown type
data U f = forall t . U (f t)
data U2 f g = forall t . U2 (f t) (g t)

--------------------------------------------------------------------------------

instance Show (Type t) where
  show Integer = "Int"
  show Bool    = "Bool"
  show Real    = "Real"

--------------------------------------------------------------------------------
