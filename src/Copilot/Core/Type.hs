--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Typing for Core.

{-# LANGUAGE Safe #-}
{-# LANGUAGE  ExistentialQuantification
            , GADTs
            , KindSignatures
            , ScopedTypeVariables
            , UndecidableInstances
            , FlexibleContexts
            , DataKinds
#-}

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
  ) where

import Data.Int
import Data.Word
import Copilot.Core.Type.Equality
import Copilot.Core.Type.Array

import Data.Typeable (Typeable, typeRep)

import GHC.TypeLits (KnownNat, natVal, Symbol, KnownSymbol, symbolVal)
import Data.Proxy   (Proxy (..))


class Struct a where
  typename :: a -> String
  toValues :: a -> [Value a]

data Value a = forall s t. (Typeable t, KnownSymbol s) => Value (Type t) (Field s t)

data Field (s :: Symbol) t = Show t => Field t

fieldname :: forall s t. KnownSymbol s => Field s t -> String
fieldname _ = symbolVal (Proxy :: Proxy s)

accessorname :: forall a s t. (Struct a, KnownSymbol s) => (a -> Field s t) -> String
accessorname _ = symbolVal (Proxy :: Proxy s)


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
  Array   :: forall n t. ( KnownNat n
                         , Typed t
                         , Typed (InnerType t)
                         , Flatten t (InnerType t)
                         ) => Type t -> Type (Array n t)
  Struct  :: (Show a, Typed a, Struct a) => a -> Type a

-- Get the length of an array from its type
tylength :: forall n t. KnownNat n => Type (Array n t) -> Int
tylength _ = fromIntegral $ natVal (Proxy :: Proxy n)

-- Get the total (nested) size of an array from its type
tysize :: forall n t. KnownNat n => Type (Array n t) -> Int
tysize ty@(Array ty'@(Array _)) = tylength ty * tylength ty'
tysize ty@(Array _            ) = tylength ty

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
  SStruct :: SimpleType

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
  SFloat  == SFloat   = True
  SDouble == SDouble  = True
  (SArray t1) == (SArray t2) | Just Refl <- t1 =~= t2 = True
                             | otherwise              = False
  SStruct == SStruct  = True
  _ == _ = False

--------------------------------------------------------------------------------

class (Show a, Typeable a) => Typed a where
  typeOf     :: Type a
  simpleType :: Type a -> SimpleType
  simpleType _ = SStruct

--------------------------------------------------------------------------------

instance Typed Bool   where
  typeOf       = Bool
  simpleType _ = SBool
instance Typed Int8   where
  typeOf       = Int8
  simpleType _ = SBool
instance Typed Int16  where
  typeOf       = Int16
  simpleType _ = SInt16
instance Typed Int32  where
  typeOf       = Int32
  simpleType _ = SInt32
instance Typed Int64  where
  typeOf       = Int64
  simpleType _ = SInt64
instance Typed Word8  where
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
instance Typed Float  where
  typeOf       = Float
  simpleType _ = SFloat
instance Typed Double where
  typeOf       = Double
  simpleType _ = SDouble
instance (Typeable t, Typed t, KnownNat n, Flatten t (InnerType t), Typed (InnerType t)) => Typed (Array n t) where
  typeOf                = Array typeOf
  simpleType (Array t)  = SArray t

--------------------------------------------------------------------------------

-- | A untyped type (no phantom type).
data UType = forall a . Typeable a => UType { uTypeType :: Type a }

instance Eq UType where
  UType ty1 == UType ty2 = typeRep ty1 == typeRep ty2

--------------------------------------------------------------------------------
