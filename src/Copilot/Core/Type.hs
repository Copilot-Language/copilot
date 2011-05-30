-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | 

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Type
  ( Type (..)
  , Typed (..)
  ) where

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
  | forall β γ .     Tup2T (Equal α (β, γ)) (Type β) (Type γ)
  | forall β γ δ .   Tup3T (Equal α (β, γ, δ)) (Type β) (Type γ) (Type δ)
  | forall β γ δ ξ . Tup4T (Equal α (β, γ, δ, ξ))
      (Type β) (Type γ) (Type δ) (Type ξ)

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
  (=~=) (Tup2T x ta1 tb1) (Tup2T y ta2 tb2) =
    do
      p <- ta1 =~= ta2
      q <- tb1 =~= tb2
      return $ trans (trans x (cong2 p q)) (symm y)
  (=~=) (Tup3T x ta1 tb1 tc1) (Tup3T y ta2 tb2 tc2) =
    do
      p <- ta1 =~= ta2
      q <- tb1 =~= tb2
      r <- tc1 =~= tc2
      return $ trans (trans x (cong3 p q r)) (symm y)
  (=~=) (Tup4T x ta1 tb1 tc1 td1) (Tup4T y ta2 tb2 tc2 td2) =
    do
      p <- ta1 =~= ta2
      q <- tb1 =~= tb2
      r <- tc1 =~= tc2
      w <- td1 =~= td2
      return $ trans (trans x (cong4 p q r w)) (symm y)
  (=~=) _ _ = Nothing

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

instance (Typed α, Typed β) => Typed (α, β) where
  typeOf = Tup2T (mkEqual id) typeOf typeOf

instance (Typed α, Typed β, Typed γ) => Typed (α, β, γ) where
  typeOf = Tup3T (mkEqual id) typeOf typeOf typeOf

instance (Typed α, Typed β, Typed γ, Typed δ) => Typed (α, β, γ, δ) where
  typeOf = Tup4T (mkEqual id) typeOf typeOf typeOf typeOf
