{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE Safe                      #-}

-- | Types suported by the modular transition systems.
module Copilot.Theorem.TransSys.Type
  ( Type (..)
  , U (..)
  ) where

import Data.Type.Equality ((:~:)(..), testEquality, TestEquality) 

-- | A type at both value and type level.
--
-- Real numbers are mapped to 'Double's.
data Type a where
  Bool    :: Type Bool
  Integer :: Type Integer
  Real    :: Type Double

-- | Proofs of type equality.
instance TestEquality Type where
  testEquality Bool  Bool  = Just Refl
  testEquality Integer  Integer  = Just Refl
  testEquality Real Real = Just Refl
  testEquality _ _ = Nothing

-- | Unknown types.
--
-- For instance, 'U Expr' is the type of an expression of unknown type
data U f = forall t . U (f t)

instance Show (Type t) where
  show Integer = "Int"
  show Bool    = "Bool"
  show Real    = "Real"
