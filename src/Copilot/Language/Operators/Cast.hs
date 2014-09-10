--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Type-safe casting operators.

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Copilot.Language.Operators.Cast 
  ( cast ) where

import qualified Copilot.Core.Operators as C
import Copilot.Core.Type
import Copilot.Language.Stream

import Data.Int
import Data.Word

--------------------------------------------------------------------------------

class Cast a b where
  cast :: (Typed a, Typed b) => Stream a -> Stream b

--------------------------------------------------------------------------------

castBool :: (Eq a, Num a, Typed a) => Stream Bool -> Stream a
castBool (Const bool) = Const $ if bool then 1 else 0
castBool x            = Op3 (C.Mux typeOf) x 1 0

--------------------------------------------------------------------------------

instance Cast Bool Bool where
  cast = id
instance Cast Bool Word8 where
  cast = castBool
instance Cast Bool Word16 where
  cast = castBool
instance Cast Bool Word32 where
  cast = castBool
instance Cast Bool Word64 where
  cast = castBool

instance Cast Bool Int8 where
  cast = castBool
instance Cast Bool Int16 where
  cast = castBool
instance Cast Bool Int32 where
  cast = castBool
instance Cast Bool Int64 where
  cast = castBool

--------------------------------------------------------------------------------

castIntegral :: (Integral a, Typed a, Num b, Typed b) => Stream a -> Stream b
castIntegral (Const x) = Const (fromIntegral x)
castIntegral x         = Op1 (C.Cast typeOf typeOf) x

--------------------------------------------------------------------------------

instance Cast Word8 Word8 where
  cast = castIntegral
instance Cast Word8 Word16 where
  cast = castIntegral
instance Cast Word8 Word32 where
  cast = castIntegral
instance Cast Word8 Word64 where
  cast = castIntegral

instance Cast Word8 Int16 where
  cast = castIntegral
instance Cast Word8 Int32 where
  cast = castIntegral
instance Cast Word8 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

instance Cast Word16 Word16 where
  cast = castIntegral
instance Cast Word16 Word32 where
  cast = castIntegral
instance Cast Word16 Word64 where
  cast = castIntegral

instance Cast Word16 Int32 where
  cast = castIntegral
instance Cast Word16 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

instance Cast Word32 Word32 where
  cast = castIntegral
instance Cast Word32 Word64 where
  cast = castIntegral

instance Cast Word32 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

instance Cast Word64 Word64 where
  cast = castIntegral

--------------------------------------------------------------------------------

instance Cast Int8 Int8 where
  cast = castIntegral
instance Cast Int8 Int16 where
  cast = castIntegral
instance Cast Int8 Int32 where
  cast = castIntegral
instance Cast Int8 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

instance Cast Int16 Int16 where
  cast = castIntegral
instance Cast Int16 Int32 where
  cast = castIntegral
instance Cast Int16 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

instance Cast Int32 Int32 where
  cast = castIntegral
instance Cast Int32 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

instance Cast Int64 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

