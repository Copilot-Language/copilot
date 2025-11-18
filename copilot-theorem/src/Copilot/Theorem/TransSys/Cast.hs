{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Casting of values with dynamic types and translating from Copilot core
-- types to Copilot theorem types.

module Copilot.Theorem.TransSys.Cast
  ( Dyn
  , toDyn
  , cast
  , castedType
  , casting
  ) where

import Copilot.Core as C

import Data.Dynamic (Dynamic(..), fromDynamic, toDyn)
import GHC.Float

import qualified Copilot.Theorem.TransSys.Type as K

-- | Synonym for a dynamic type in Copilot core.
type Dyn = Dynamic

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
  o       -> error $ "There is a bug in type checker " ++ show o

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

class Casted b where
  _cast :: Dyn -> Maybe b

instance Casted Integer where
  _cast d
    | Just (v :: Int8)   <- fromDynamic d = Just $ toInteger v
    | Just (v :: Int16)  <- fromDynamic d = Just $ toInteger v
    | Just (v :: Int32)  <- fromDynamic d = Just $ toInteger v
    | Just (v :: Int64)  <- fromDynamic d = Just $ toInteger v
    | Just (v :: Word8)  <- fromDynamic d = Just $ toInteger v
    | Just (v :: Word16) <- fromDynamic d = Just $ toInteger v
    | Just (v :: Word32) <- fromDynamic d = Just $ toInteger v
    | Just (v :: Word64) <- fromDynamic d = Just $ toInteger v
    | otherwise                           = Nothing

instance Casted Bool where
  _cast d
    | Just (v :: Bool) <- fromDynamic d = Just v
    | otherwise                         = Nothing

instance Casted Double where
  _cast d
    | Just (v :: Float)  <- fromDynamic d = Just $ float2Double v
    | Just (v :: Double) <- fromDynamic d = Just v
    | otherwise                           = Nothing
