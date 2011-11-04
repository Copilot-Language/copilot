--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | 

{-# LANGUAGE ExistentialQuantification, GADTs, KindSignatures #-}

module Copilot.Core.Type
  ( Type (..)
  , Typed (..)
  , UType (..)
  ) where

import Data.Int
import Data.Word
import Copilot.Core.Type.Equality

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

class Typed a where
  typeOf :: Type a

instance Typed Bool   where typeOf = Bool
instance Typed Int8   where typeOf = Int8
instance Typed Int16  where typeOf = Int16
instance Typed Int32  where typeOf = Int32
instance Typed Int64  where typeOf = Int64
instance Typed Word8  where typeOf = Word8
instance Typed Word16 where typeOf = Word16
instance Typed Word32 where typeOf = Word32
instance Typed Word64 where typeOf = Word64
instance Typed Float  where typeOf = Float
instance Typed Double where typeOf = Double

--------------------------------------------------------------------------------

-- | A untyped type (no phantom type).
data UType = forall a . UType { uTypeType :: Type a }
