{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Safe                      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
-- |
-- Description: Typing for Core.
-- Copyright:   (c) 2011 National Institute of Aerospace / Galois, Inc.
--
-- All expressions and streams in Core are accompanied by a representation of
-- the types of the underlying expressions used or carried by the streams.
-- This information is needed by the compiler to generate code, since it must
-- initialize variables and equivalent representations for those types in
-- the target languages.
module Copilot.Core.Type
    ( Type (..)
    , Typed (..)
    , UType (..)
    , SimpleType (..)

    , tysize
    , tylength

    , Value (..)
    , toValues
    , Field (..)
    , typename

    , Struct
    , fieldname
    , accessorname
    )
  where

-- External imports
import Data.Int           (Int16, Int32, Int64, Int8)
import Data.List          (intercalate)
import Data.Proxy         (Proxy (..))
import Data.Type.Equality as DE
import Data.Typeable      (Typeable, eqT, typeRep)
import Data.Word          (Word16, Word32, Word64, Word8)
import GHC.TypeLits       (KnownNat, KnownSymbol, Symbol, natVal, sameNat,
                           symbolVal)

-- Internal imports
import Copilot.Core.Type.Array (Array)

-- | The value of that is a product or struct, defined as a constructor with
-- several fields.
class Struct a where
  -- | Returns the name of struct in the target language.
  typename :: a -> String
  -- | Transforms all the struct's fields into a list of values.
  toValues :: a -> [Value a]

-- | The field of a struct, together with a representation of its type.
data Value a =
  forall s t . (Typeable t, KnownSymbol s, Show t) => Value (Type t) (Field s t)

-- | A field in a struct. The name of the field is a literal at the type
-- level.
data Field (s :: Symbol) t = Field t

-- | Extract the name of a field.
fieldname :: forall s t . KnownSymbol s => Field s t -> String
fieldname _ = symbolVal (Proxy :: Proxy s)

-- | Extract the name of an accessor (a function that returns a field of a
-- struct).
accessorname :: forall a s t . (Struct a, KnownSymbol s)
             => (a -> Field s t) -> String
accessorname _ = symbolVal (Proxy :: Proxy s)

instance (KnownSymbol s, Show t) => Show (Field s t) where
  show f@(Field v) = fieldname f ++ ":" ++ show v

instance {-# OVERLAPPABLE #-} (Typed t, Struct t) => Show t where
  show t = "<" ++ fields ++ ">"
    where
      fields = intercalate "," $ map showfield (toValues t)
      showfield (Value _ field) = show field

-- | A Type representing the types of expressions or values handled by
-- Copilot Core.
--
-- Note that both arrays and structs use dependently typed features. In the
-- former, the length of the array is part of the type. In the latter, the
-- names of the fields are part of the type.
data Type :: * -> * where
  Bool   :: Type Bool
  Int8   :: Type Int8
  Int16  :: Type Int16
  Int32  :: Type Int32
  Int64  :: Type Int64
  Word8  :: Type Word8
  Word16 :: Type Word16
  Word32 :: Type Word32
  Word64 :: Type Word64
  Float  :: Type Float
  Double :: Type Double
  Array  :: forall n t . ( KnownNat n
                         , Typed t
                         ) => Type t -> Type (Array n t)
  Struct :: (Typed a, Struct a) => a -> Type a

-- | Return the length of an array from its type
tylength :: forall n t . KnownNat n => Type (Array n t) -> Int
tylength _ = fromIntegral $ natVal (Proxy :: Proxy n)

-- | Return the total (nested) size of an array from its type
tysize :: forall n t . KnownNat n => Type (Array n t) -> Int
tysize ty@(Array ty'@(Array _)) = tylength ty * tysize ty'
tysize ty@(Array _            ) = tylength ty

instance TestEquality Type where
  testEquality Bool   Bool   = Just DE.Refl
  testEquality Int8   Int8   = Just DE.Refl
  testEquality Int16  Int16  = Just DE.Refl
  testEquality Int32  Int32  = Just DE.Refl
  testEquality Int64  Int64  = Just DE.Refl
  testEquality Word8  Word8  = Just DE.Refl
  testEquality Word16 Word16 = Just DE.Refl
  testEquality Word32 Word32 = Just DE.Refl
  testEquality Word64 Word64 = Just DE.Refl
  testEquality Float  Float  = Just DE.Refl
  testEquality Double Double = Just DE.Refl
  testEquality (Array t1) (Array t2) =
      testArrayEquality t1 t2
    where
      testArrayEquality :: forall n1 a1 n2 a2.
                           (KnownNat n1, KnownNat n2)
                        => Type a1
                        -> Type a2
                        -> Maybe (Array n1 a1 :~: Array n2 a2)
      testArrayEquality ty1 ty2
        | Just DE.Refl <- sameNat (Proxy :: Proxy n1) (Proxy :: Proxy n2)
        , Just DE.Refl <- testEquality ty1 ty2
        = Just DE.Refl
        | otherwise
        = Nothing
  testEquality (Struct _) (Struct _) = eqT
  testEquality _ _ = Nothing

-- | A simple, monomorphic representation of types that facilitates putting
-- variables in heterogeneous lists and environments in spite of their types
-- being different.
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
  SStruct :: SimpleType

-- | Type equality, used to help type inference.

-- This instance is necessary, otherwise the type of SArray can't be inferred.
instance Eq SimpleType where
  SBool   == SBool   = True
  SInt8   == SInt8   = True
  SInt16  == SInt16  = True
  SInt32  == SInt32  = True
  SInt64  == SInt64  = True
  SWord8  == SWord8  = True
  SWord16 == SWord16 = True
  SWord32 == SWord32 = True
  SWord64 == SWord64 = True
  SFloat  == SFloat  = True
  SDouble == SDouble = True
  (SArray t1) == (SArray t2) | Just DE.Refl <- testEquality t1 t2 = True
                             | otherwise                          = False
  SStruct == SStruct = True
  _ == _ = False

-- | A typed expression, from which we can obtain the two type representations
-- used by Copilot: 'Type' and 'SimpleType'.
class (Show a, Typeable a) => Typed a where
  typeOf     :: Type a
  simpleType :: Type a -> SimpleType
  simpleType _ = SStruct

instance Typed Bool where
  typeOf       = Bool
  simpleType _ = SBool

instance Typed Int8 where
  typeOf       = Int8
  simpleType _ = SInt8

instance Typed Int16 where
  typeOf       = Int16
  simpleType _ = SInt16

instance Typed Int32 where
  typeOf       = Int32
  simpleType _ = SInt32

instance Typed Int64 where
  typeOf       = Int64
  simpleType _ = SInt64

instance Typed Word8 where
  typeOf       = Word8
  simpleType _ = SWord8

instance Typed Word16 where
  typeOf       = Word16
  simpleType _ = SWord16

instance Typed Word32 where
  typeOf       = Word32
  simpleType _ = SWord32

instance Typed Word64 where
  typeOf       = Word64
  simpleType _ = SWord64

instance Typed Float where
  typeOf       = Float
  simpleType _ = SFloat

instance Typed Double where
  typeOf       = Double
  simpleType _ = SDouble

instance (Typeable t, Typed t, KnownNat n) => Typed (Array n t) where
  typeOf               = Array typeOf
  simpleType (Array t) = SArray t

-- | A untyped type (no phantom type).
data UType = forall a . Typeable a => UType { uTypeType :: Type a }

instance Eq UType where
  UType ty1 == UType ty2 = typeRep ty1 == typeRep ty2
