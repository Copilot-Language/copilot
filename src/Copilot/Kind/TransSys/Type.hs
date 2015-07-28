--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Copilot.Kind.TransSys.Type
  ( Type (..)
  , U (..)
  , U2 (..)
  ) where

import Copilot.Core.Type.Equality

--------------------------------------------------------------------------------

data Type a where
  Bool    :: Type Bool
  Integer :: Type Integer
  Real    :: Type Double

instance EqualType Type where
  Bool    =~= Bool     = Just Refl
  Integer =~= Integer  = Just Refl
  Real    =~= Real     = Just Refl
  _       =~= _        = Nothing

--------------------------------------------------------------------------------

-- For instance, 'U Expr' is the type of an expression of unknown type

data U f = forall t . U (f t)
data U2 f g = forall t . U2 (f t) (g t)

--------------------------------------------------------------------------------

instance Show (Type t) where
  show Integer = "Int"
  show Bool    = "Bool"
  show Real    = "Real"

--------------------------------------------------------------------------------
