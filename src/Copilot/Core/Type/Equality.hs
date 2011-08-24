--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, KindSignatures #-}

module Copilot.Core.Type.Equality
  ( Equal (..)
  , EqualType (..)
  , coerce
  , refl
  , trans
  , symm
  , cong
  ) where

data Equal :: * -> * -> * where
  Refl :: Equal a a

class EqualType t where
  (=~=) :: t a -> t b -> Maybe (Equal a b)

coerce :: Equal a b -> a -> b
coerce Refl x = x

refl :: Equal a a
refl = Refl

symm :: Equal a b -> Equal b a
symm Refl = Refl

trans :: Equal a b -> Equal b c -> Equal a c
trans Refl Refl = Refl

cong :: Equal a b -> Equal (f a) (f b)
cong Refl = Refl
