--------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE Safe #-}

-- | Casting of values with dynamic types and translating from Copilot core
-- types to Copilot theorem types.

module Copilot.Theorem.TransSys.Cast
  ( Dyn
  , toDyn
  , cast
  , castedType
  , casting
  ) where

--------------------------------------------------------------------------------

import Copilot.Core as C
import Copilot.Core.Type.Equality
import Copilot.Core.Type.Dynamic

import GHC.Float

import qualified Copilot.Theorem.TransSys.Type as K

--------------------------------------------------------------------------------

-- | Synonym for a dynamic type in Copilot core.
type Dyn = Dynamic Type

-- | Translation of a Copilot type into Copilot theorem's internal
-- representation.
castedType :: Type t -> K.U K.Type
castedType t = case t of
  Bool    -> K.U K.Bool
  Int8    -> K.U K.Integer
  Int16   -> K.U K.Integer
  Int32   -> K.U K.Integer
  Int64   -> K.U K.Integer
  Word8   -> K.U K.Integer
  Word16  -> K.U K.Integer
  Word32  -> K.U K.Integer
  Word64  -> K.U K.Integer
  Float   -> K.U K.Real
  Double  -> K.U K.Real

-- | Cast a dynamic value to a given type.
cast :: K.Type t -> Dyn -> t
cast t v
  | K.Integer <- t,  Just (vi :: Integer) <- _cast v = vi
  | K.Bool    <- t,  Just (vb :: Bool)    <- _cast v = vb
  | K.Real    <- t,  Just (vr :: Double)  <- _cast v = vr
  | otherwise = error "Bad type cast"

-- | Apply function to a corresponding type in Copilot theorem's internal
-- representation.
casting :: Type t -> (forall t' . K.Type t' -> a) -> a
casting t f = case castedType t of
  K.U K.Bool    -> f K.Bool
  K.U K.Integer -> f K.Integer
  K.U K.Real    -> f K.Real

--------------------------------------------------------------------------------

class Casted b where
  _cast :: Dyn -> Maybe b

instance Casted Integer where
  _cast (Dynamic v tv)
    | Just Refl <- tv =~= Int8    = Just $ toInteger v
    | Just Refl <- tv =~= Int16   = Just $ toInteger v
    | Just Refl <- tv =~= Int32   = Just $ toInteger v
    | Just Refl <- tv =~= Int64   = Just $ toInteger v
    | Just Refl <- tv =~= Word16  = Just $ toInteger v
    | Just Refl <- tv =~= Word8   = Just $ toInteger v
    | Just Refl <- tv =~= Word32  = Just $ toInteger v
    | Just Refl <- tv =~= Word64  = Just $ toInteger v
    | otherwise                   = Nothing

instance Casted Bool where
  _cast (Dynamic v tv)
    | Just Refl <- tv =~= Bool  = Just v
    | otherwise                 = Nothing

instance Casted Double where
  _cast (Dynamic v tv)
    | Just Refl <- tv =~= Float  = Just $ float2Double v
    | Just Refl <- tv =~= Double = Just v
    | otherwise = Nothing

--------------------------------------------------------------------------------
