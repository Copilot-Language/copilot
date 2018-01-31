--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Typing for Core.

{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification, GADTs, KindSignatures #-}

module Copilot.Core.Type
  ( Type (..)
  , Typed (..)
  , UType (..)
  , SimpleType (..)
  ) where

import Data.Int
import Data.Word
import Copilot.Core.Type.Equality
import Copilot.Core.Type.Array


{- Class and instances of elements that are allowed in arrays -}
class Typed t => ArrayItem t
instance ArrayItem Bool
instance ArrayItem Int8
instance ArrayItem Int16
instance ArrayItem Int32
instance ArrayItem Int64
instance ArrayItem Word8
instance ArrayItem Word16
instance ArrayItem Word32
instance ArrayItem Word64
instance ArrayItem Float
instance ArrayItem Double

data Type :: * -> * where
  Bool    :: Type Bool
  Int8    :: Type Int8
  Int16   :: Type Int16
  Int32   :: Type Int32
  Int64   :: Type Int64
  Word8   :: Type Word8
  Word16  :: Type Word16
  Word32  :: Type Word32
  Word64  :: Type Word64
  Float   :: Type Float
  Double  :: Type Double
  Array   :: (ArrayItem t, Index i) => Type t -> Type (Array i t)

instance EqualType Type where
  (=~=) Bool   Bool   = Just Refl
  (=~=) Int8   Int8   = Just Refl
  (=~=) Int16  Int16  = Just Refl
  (=~=) Int32  Int32  = Just Refl
  (=~=) Int64  Int64  = Just Refl
  (=~=) Word8  Word8  = Just Refl
  (=~=) Word16 Word16 = Just Refl
  (=~=) Word32 Word32 = Just Refl
  (=~=) Word64 Word64 = Just Refl
  (=~=) Float  Float  = Just Refl
  (=~=) Double Double = Just Refl
  {-(=~=) (Array t1) (Array t2) | Just Refl <- t1 =~= t2 = Just Refl
                              | otherwise              = Nothing-}
  (=~=) _ _ = Nothing

--------------------------------------------------------------------------------

data SimpleType where
  SBool   :: SimpleType
  SInt8   :: SimpleType
  SInt16  :: SimpleType
  SInt32  :: SimpleType
  SInt64  :: SimpleType
  SWord8  :: SimpleType
  SWord16 :: SimpleType
  SWord32 :: SimpleType
  SWord64 :: SimpleType
  SFloat  :: SimpleType
  SDouble :: SimpleType
  SArray  :: Type t -> SimpleType

{- This instance is necessary, otherwise the type of SArray can't be inferred -}
instance Eq SimpleType where
  SBool   == SBool    = True
  SInt8   == SInt8    = True
  SInt16  == SInt16   = True
  SInt32  == SInt32   = True
  SInt64  == SInt64   = True
  SWord8  == SWord8   = True
  SWord16 == SWord16  = True
  SWord32 == SWord32  = True
  SWord64 == SWord64  = True
  (SArray t1) == (SArray t2) | Just Refl <- t1 =~= t2 = True
                             | otherwise              = False
  _ == _ = False

--------------------------------------------------------------------------------

class Typed a where
  typeOf     :: Type a
  simpleType :: Type a -> SimpleType
  sizeOf     :: a -> Int

--------------------------------------------------------------------------------

instance Typed Bool   where
  typeOf       = Bool
  simpleType _ = SBool
  sizeOf _     = 1
instance Typed Int8   where
  typeOf       = Int8
  simpleType _ = SBool
  sizeOf _     = 1
instance Typed Int16  where
  typeOf       = Int16
  simpleType _ = SInt16
  sizeOf _     = 1
instance Typed Int32  where
  typeOf       = Int32
  simpleType _ = SInt32
  sizeOf _     = 1
instance Typed Int64  where
  typeOf       = Int64
  simpleType _ = SInt64
  sizeOf _     = 1
instance Typed Word8  where
  typeOf       = Word8
  simpleType _ = SWord8
  sizeOf _     = 1
instance Typed Word16 where
  typeOf       = Word16
  simpleType _ = SWord16
  sizeOf _     = 1
instance Typed Word32 where
  typeOf       = Word32
  simpleType _ = SWord32
  sizeOf _     = 1
instance Typed Word64 where
  typeOf       = Word64
  simpleType _ = SWord64
  sizeOf _     = 1
instance Typed Float  where
  typeOf       = Float
  simpleType _ = SFloat
  sizeOf _     = 1
instance Typed Double where
  typeOf       = Double
  simpleType _ = SDouble
  sizeOf _     = 1
instance (Index i, ArrayItem t) => Typed (Array i t) where
  typeOf                = Array typeOf
  simpleType (Array t)  = SArray t
  sizeOf a              = length $ indices $ dim a

--------------------------------------------------------------------------------

-- | A untyped type (no phantom type).
data UType = forall a . UType { uTypeType :: Type a }

--------------------------------------------------------------------------------
