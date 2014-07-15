--------------------------------------------------------------------------------

module Copilot.Kind.Misc.Type       
  ( Type (..)
  , U (..)
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
  
data U f = forall t . U (f t)

--------------------------------------------------------------------------------

instance Show (Type t) where
  show Integer = "Int"
  show Bool    = "Bool"
  show Real    = "Real"

--------------------------------------------------------------------------------
