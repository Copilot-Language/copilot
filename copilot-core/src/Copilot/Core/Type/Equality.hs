--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Safe #-}

-- | Propositional equality and type equality.
module Copilot.Core.Type.Equality
  ( Equal (..)
  , EqualType (..)
  , coerce
  , refl
  , trans
  , symm
  , cong
  ) where

-- | Propositional equality.
data Equal :: * -> * -> * where
  Refl :: Equal a a

-- | Type equality. If the value of @x =~= y@ is @Just Refl@, then the two
-- types @x@ and @y@ are equal.
class EqualType t where
  (=~=) :: t a -> t b -> Maybe (Equal a b)

-- | Safe coercion, which requires proof of equality.
coerce :: Equal a b -> a -> b
coerce Refl x = x

-- | Proof of propositional equality.
refl :: Equal a a
refl = Refl

-- | Symmetry.
symm :: Equal a b -> Equal b a
symm Refl = Refl

-- | Transitivity.
trans :: Equal a b -> Equal b c -> Equal a c
trans Refl Refl = Refl

-- | Congruence.
cong :: Equal a b -> Equal (f a) (f b)
cong Refl = Refl
