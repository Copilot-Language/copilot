-- | Defines Copilot types. Must be in its own module
-- as HeteroMap depends on HasType.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Copilot.Core.Type
  ( module Data.Int
  , module Data.Word
  , Type (..)
  , HasType (..)
  , Streamable
  ) where

import Control.DeepSeq (NFData)
import Data.Int
import Data.Type.Equality (EqT (eqT), (:=:) (Refl))
import Data.Word

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

deriving instance Eq (Type a)

deriving instance Show (Type a)

class HasType a where typeOf :: Type a

instance HasType Bool   where typeOf = Bool
instance HasType Int8   where typeOf = Int8
instance HasType Int16  where typeOf = Int16
instance HasType Int32  where typeOf = Int32
instance HasType Int64  where typeOf = Int64
instance HasType Word8  where typeOf = Word8
instance HasType Word16 where typeOf = Word16
instance HasType Word32 where typeOf = Word32
instance HasType Word64 where typeOf = Word64
instance HasType Float  where typeOf = Float
instance HasType Double where typeOf = Double

instance EqT Type where
  eqT Bool   Bool   = Just Refl
  eqT Int8   Int8   = Just Refl
  eqT Int16  Int16  = Just Refl
  eqT Int32  Int32  = Just Refl
  eqT Int64  Int64  = Just Refl
  eqT Word8  Word8  = Just Refl
  eqT Word16 Word16 = Just Refl
  eqT Word32 Word32 = Just Refl
  eqT Word64 Word64 = Just Refl
  eqT Float  Float  = Just Refl
  eqT Double Double = Just Refl
  eqT _      _      = Nothing

class
  ( NFData a
  , Eq a
  , Show a
  , HasType a
  ) => Streamable a

instance Streamable Bool
instance Streamable Int8
instance Streamable Int16
instance Streamable Int32
instance Streamable Int64
instance Streamable Word8
instance Streamable Word16
instance Streamable Word32
instance Streamable Word64
instance Streamable Float
instance Streamable Double
