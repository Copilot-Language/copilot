--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | 

module Copilot.Core.Type
  ( Type (..)
  , Typed (..)
  , UType (..)
  , utype
  ) where

import Copilot.Core.Type.Equality
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

--------------------------------------------------------------------------------

data Type a
  = Bool   (Equal a Bool)
  | Int8   (Equal a Int8)
  | Int16  (Equal a Int16)
  | Int32  (Equal a Int32)
  | Int64  (Equal a Int64)
  | Word8  (Equal a Word8)
  | Word16 (Equal a Word16)
  | Word32 (Equal a Word32)
  | Word64 (Equal a Word64)
  | Float  (Equal a Float)
  | Double (Equal a Double)

instance EqualType Type where
  (=~=) (Bool x)   (Bool y)   = Just (trans x (symm y))
  (=~=) (Int8 x)   (Int8 y)   = Just (trans x (symm y))
  (=~=) (Int16 x)  (Int16 y)  = Just (trans x (symm y))
  (=~=) (Int32 x)  (Int32 y)  = Just (trans x (symm y))
  (=~=) (Int64 x)  (Int64 y)  = Just (trans x (symm y))
  (=~=) (Word8 x)  (Word8 y)  = Just (trans x (symm y))
  (=~=) (Word16 x) (Word16 y) = Just (trans x (symm y))
  (=~=) (Word32 x) (Word32 y) = Just (trans x (symm y))
  (=~=) (Word64 x) (Word64 y) = Just (trans x (symm y))
  (=~=) (Float x)  (Float y)  = Just (trans x (symm y))
  (=~=) (Double x) (Double y) = Just (trans x (symm y))
  (=~=) _ _ = Nothing

--------------------------------------------------------------------------------

class Typed a where
  typeOf :: Type a

instance Typed Bool   where typeOf = Bool   (mkEqual id)
instance Typed Int8   where typeOf = Int8   (mkEqual id)
instance Typed Int16  where typeOf = Int16  (mkEqual id)
instance Typed Int32  where typeOf = Int32  (mkEqual id)
instance Typed Int64  where typeOf = Int64  (mkEqual id)
instance Typed Word8  where typeOf = Word8  (mkEqual id)
instance Typed Word16 where typeOf = Word16 (mkEqual id)
instance Typed Word32 where typeOf = Word32 (mkEqual id)
instance Typed Word64 where typeOf = Word64 (mkEqual id)
instance Typed Float  where typeOf = Float  (mkEqual id)
instance Typed Double where typeOf = Double (mkEqual id)

--------------------------------------------------------------------------------

-- Raw 'untyped' types:

data UType
  = UBool
  | UInt8
  | UInt16
  | UInt32
  | UInt64
  | UWord8
  | UWord16
  | UWord32
  | UWord64
  | UFloat
  | UDouble

utype :: Type a -> UType
utype (Bool  _)  = UBool
utype (Int8  _)  = UInt8
utype (Int16 _)  = UInt16
utype (Int32 _)  = UInt32
utype (Int64 _)  = UInt64
utype (Word8  _) = UWord8
utype (Word16 _) = UWord16
utype (Word32 _) = UWord32
utype (Word64 _) = UWord64
utype (Float  _) = UFloat
utype (Double _) = UDouble

--------------------------------------------------------------------------------