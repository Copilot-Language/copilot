-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | 

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Type
  ( module Data.Int
  , module Data.Word
  , Type (..)
  , Typed (..)
  , Uninitialized (..)
  , Streamable
  ) where

import Control.DeepSeq (NFData)
import Data.Int
import Data.Word
import Copilot.Core.Type.Equality

data Type α
  = BoolT   (Equal α Bool)
  | Int8T   (Equal α Int8)
  | Int16T  (Equal α Int16)
  | Int32T  (Equal α Int32)
  | Int64T  (Equal α Int64)
  | Word8T  (Equal α Word8)
  | Word16T (Equal α Word16)
  | Word32T (Equal α Word32)
  | Word64T (Equal α Word64)
  | FloatT  (Equal α Float)
  | DoubleT (Equal α Double)

class Typed α where
  typeOf :: Type α

instance Typed Bool   where typeOf = BoolT   (mkEqual id)
instance Typed Int8   where typeOf = Int8T   (mkEqual id)
instance Typed Int16  where typeOf = Int16T  (mkEqual id)
instance Typed Int32  where typeOf = Int32T  (mkEqual id)
instance Typed Int64  where typeOf = Int64T  (mkEqual id)
instance Typed Word8  where typeOf = Word8T  (mkEqual id)
instance Typed Word16 where typeOf = Word16T (mkEqual id)
instance Typed Word32 where typeOf = Word32T (mkEqual id)
instance Typed Word64 where typeOf = Word64T (mkEqual id)
instance Typed Float  where typeOf = FloatT  (mkEqual id)
instance Typed Double where typeOf = DoubleT (mkEqual id)

instance EqualType Type where
  (=~=) (BoolT x)   (BoolT y)   = Just (trans x (symm y))
  (=~=) (Int8T x)   (Int8T y)   = Just (trans x (symm y))
  (=~=) (Int16T x)  (Int16T y)  = Just (trans x (symm y))
  (=~=) (Int32T x)  (Int32T y)  = Just (trans x (symm y))
  (=~=) (Int64T x)  (Int64T y)  = Just (trans x (symm y))
  (=~=) (Word8T x)  (Word8T y)  = Just (trans x (symm y))
  (=~=) (Word16T x) (Word16T y) = Just (trans x (symm y))
  (=~=) (Word32T x) (Word32T y) = Just (trans x (symm y))
  (=~=) (Word64T x) (Word64T y) = Just (trans x (symm y))
  (=~=) _           _           = Nothing

class Uninitialized α where
  uninitialized :: α

instance Uninitialized Bool   where uninitialized = False
instance Uninitialized Int8   where uninitialized = 0
instance Uninitialized Int16  where uninitialized = 0
instance Uninitialized Int32  where uninitialized = 0
instance Uninitialized Int64  where uninitialized = 0
instance Uninitialized Word8  where uninitialized = 0
instance Uninitialized Word16 where uninitialized = 0
instance Uninitialized Word32 where uninitialized = 0
instance Uninitialized Word64 where uninitialized = 0

class (NFData α, Show α, Typed α, Uninitialized α) => Streamable α

instance Streamable Bool
instance Streamable Int8
instance Streamable Int16
instance Streamable Int32
instance Streamable Int64
instance Streamable Word8
instance Streamable Word16
instance Streamable Word32
instance Streamable Word64
