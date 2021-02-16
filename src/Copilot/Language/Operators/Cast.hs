--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Type-safe casting operators.
module Copilot.Language.Operators.Cast
  ( cast, unsafeCast, Cast, UnsafeCast ) where

import qualified Copilot.Core.Operators as C
import Copilot.Core.Type
import Copilot.Language.Stream

import Data.Int
import Data.Word

--------------------------------------------------------------------------------

-- | Class to capture casting between types for which it can be performed
-- safely.
class Cast a b where
  -- | Perform a safe cast from @Stream a@ to @Stream b@.
  cast :: (Typed a, Typed b) => Stream a -> Stream b

-- | Class to capture casting between types for which casting may be unsafe
-- and/or result in a loss of precision or information.
class UnsafeCast a b where
  -- | Perform an unsafe cast from @Stream a@ to @Stream b@.
  unsafeCast :: (Typed a, Typed b) => Stream a -> Stream b

--------------------------------------------------------------------------------

-- | Cast a boolean stream to a stream of numbers, producing 1 if the
-- value at a point in time is 'True', and 0 otherwise.
castBool :: (Eq a, Num a, Typed a) => Stream Bool -> Stream a
castBool (Const bool) = Const $ if bool then 1 else 0
castBool x            = Op3 (C.Mux typeOf) x 1 0

--------------------------------------------------------------------------------

-- | Identity casting.
instance Cast Bool Bool where
  cast = id

-- | Cast a boolean stream to a stream of numbers, producing 1 if the
-- value at a point in time is 'True', and 0 otherwise.
instance Cast Bool Word8 where
  cast = castBool

-- | Cast a boolean stream to a stream of numbers, producing 1 if the
-- value at a point in time is 'True', and 0 otherwise.
instance Cast Bool Word16 where
  cast = castBool

-- | Cast a boolean stream to a stream of numbers, producing 1 if the
-- value at a point in time is 'True', and 0 otherwise.
instance Cast Bool Word32 where
  cast = castBool

-- | Cast a boolean stream to a stream of numbers, producing 1 if the
-- value at a point in time is 'True', and 0 otherwise.
instance Cast Bool Word64 where
  cast = castBool

-- | Cast a boolean stream to a stream of numbers, producing 1 if the
-- value at a point in time is 'True', and 0 otherwise.
instance Cast Bool Int8 where
  cast = castBool

-- | Cast a boolean stream to a stream of numbers, producing 1 if the
-- value at a point in time is 'True', and 0 otherwise.
instance Cast Bool Int16 where
  cast = castBool

-- | Cast a boolean stream to a stream of numbers, producing 1 if the
-- value at a point in time is 'True', and 0 otherwise.
instance Cast Bool Int32 where
  cast = castBool

-- | Cast a boolean stream to a stream of numbers, producing 1 if the
-- value at a point in time is 'True', and 0 otherwise.
instance Cast Bool Int64 where
  cast = castBool

--------------------------------------------------------------------------------

-- | Cast a stream carrying numbers to an integral using 'fromIntegral'.
castIntegral :: (Integral a, Typed a, Num b, Typed b) => Stream a -> Stream b
castIntegral (Const x) = Const (fromIntegral x)
castIntegral x         = Op1 (C.Cast typeOf typeOf) x

--------------------------------------------------------------------------------

-- | Identity casting.
instance Cast Word8 Word8 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word8 Word16 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word8 Word32 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word8 Word64 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word8 Int16 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word8 Int32 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word8 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

-- | Identity casting.
instance Cast Word16 Word16 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word16 Word32 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word16 Word64 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word16 Int32 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word16 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

-- | Identity casting.
instance Cast Word32 Word32 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word32 Word64 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Word32 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

-- | Identity casting.
instance Cast Word64 Word64 where
  cast = castIntegral

--------------------------------------------------------------------------------

-- | Identity casting.
instance Cast Int8 Int8 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Int8 Int16 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Int8 Int32 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Int8 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

-- | Identity casting.
instance Cast Int16 Int16 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Int16 Int32 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Int16 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

-- | Identity casting.
instance Cast Int32 Int32 where
  cast = castIntegral

-- | Cast number to bigger type.
instance Cast Int32 Int64 where
  cast = castIntegral

--------------------------------------------------------------------------------

-- | Identity casting.
instance Cast Int64 Int64 where
  cast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Word64 Word32 where
  unsafeCast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Word64 Word16 where
  unsafeCast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Word64 Word8 where
  unsafeCast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Word32 Word16 where
  unsafeCast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Word32 Word8 where
  unsafeCast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Word16 Word8 where
  unsafeCast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Int64 Int32 where
  unsafeCast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Int64 Int16 where
  unsafeCast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Int64 Int8 where
  unsafeCast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Int32 Int16 where
  unsafeCast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Int32 Int8 where
  unsafeCast = castIntegral

-- | Unsafe downcasting to smaller sizes.
instance UnsafeCast Int16 Int8 where
  unsafeCast = castIntegral

-- | Unsafe signed integer promotion to floating point values.
instance UnsafeCast Int64 Float where
  unsafeCast = castIntegral

-- | Unsafe signed integer promotion to floating point values.
instance UnsafeCast Int32 Float where
  unsafeCast = castIntegral

-- | Unsafe signed integer promotion to floating point values.
instance UnsafeCast Int16 Float where
  unsafeCast = castIntegral

-- | Unsafe signed integer promotion to floating point values.
instance UnsafeCast Int8 Float where
  unsafeCast = castIntegral

-- | Unsafe signed integer promotion to floating point values.
instance UnsafeCast Int64 Double where
  unsafeCast = castIntegral

-- | Unsafe signed integer promotion to floating point values.
instance UnsafeCast Int32 Double where
  unsafeCast = castIntegral

-- | Unsafe signed integer promotion to floating point values.
instance UnsafeCast Int16 Double where
  unsafeCast = castIntegral

-- | Unsafe signed integer promotion to floating point values.
instance UnsafeCast Int8 Double where
  unsafeCast = castIntegral

-- | Unsafe unsigned integer promotion to floating point values.
instance UnsafeCast Word64 Float where
  unsafeCast = castIntegral

-- | Unsafe unsigned integer promotion to floating point values.
instance UnsafeCast Word32 Float where
  unsafeCast = castIntegral

-- | Unsafe unsigned integer promotion to floating point values.
instance UnsafeCast Word16 Float where
  unsafeCast = castIntegral

-- | Unsafe unsigned integer promotion to floating point values.
instance UnsafeCast Word8 Float where
  unsafeCast = castIntegral

-- | Unsafe unsigned integer promotion to floating point values.
instance UnsafeCast Word64 Double where
  unsafeCast = castIntegral

-- | Unsafe unsigned integer promotion to floating point values.
instance UnsafeCast Word32 Double where
  unsafeCast = castIntegral

-- | Unsafe unsigned integer promotion to floating point values.
instance UnsafeCast Word16 Double where
  unsafeCast = castIntegral

-- | Unsafe unsigned integer promotion to floating point values.
instance UnsafeCast Word8 Double where
  unsafeCast = castIntegral

-- | Cast from unsigned numbers to signed numbers.
instance UnsafeCast Word64 Int64 where
  unsafeCast = castIntegral

-- | Cast from unsigned numbers to signed numbers.
instance UnsafeCast Word32 Int32 where
  unsafeCast = castIntegral

-- | Cast from unsigned numbers to signed numbers.
instance UnsafeCast Word16 Int16 where
  unsafeCast = castIntegral

-- | Cast from unsigned numbers to signed numbers.
instance UnsafeCast Word8 Int8 where
  unsafeCast = castIntegral

-- | Signed to unsigned casting.
instance UnsafeCast Int64 Word64 where
  unsafeCast = castIntegral

-- | Signed to unsigned casting.
instance UnsafeCast Int32 Word32 where
  unsafeCast = castIntegral

-- | Signed to unsigned casting.
instance UnsafeCast Int16 Word16 where
  unsafeCast = castIntegral

-- | Signed to unsigned casting.
instance UnsafeCast Int8 Word8 where
  unsafeCast = castIntegral
