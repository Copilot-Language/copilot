-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | Type equality using existential and universal quantification.
-- The theory behind this technique is described the following paper:
--
-- * Baars, Arthur I. and Swierstra, S. Doaitse,
-- \"/Typing dynamic typing/\",
-- ACM SIGPLAN Notices vol. 37, p. 157-166, 2002

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Core.Type.Equality
  ( Equal (..)
  , EqualType (..)
  , reflex
  , trans
  , subst
  , symm
  , cong
--  , rewrite
  , coerce
  ) where

-- | Type equality. A value of Equal a b is a proof that a is equal to b.
newtype Equal α β = CoerceF (forall f . f α -> f β)

-- | A type class for constructing equality proofs.
class EqualType τ where
  (=~=) :: τ α -> τ β -> Maybe (Equal α β)

-- | Equality is reflexive.
reflex :: Equal α α
reflex = CoerceF id

-- | Equality is transitive.
trans :: Equal α β -> Equal β γ -> Equal α γ
trans (CoerceF f) (CoerceF g) = CoerceF (g . f)

-- | Equality is substitutive.
subst :: (fα -> f α) -> (f β -> fβ) -> Equal α β -> fα -> fβ
subst from to (CoerceF ab) = to . ab . from

newtype FlipEqual y x = Flip { unFlip :: Equal x y }

-- | Equality is symmetric.
symm :: Equal α β -> Equal β α
symm ab = (subst Flip unFlip ab) reflex

newtype Comp g f x = Comp { unComp :: g (f x) }

-- | Equality is congruential.
cong :: Equal α β -> Equal (f α) (f β)
cong ab = CoerceF (subst Comp unComp ab)

--rewrite :: Equal a b -> Equal c (f a) -> Equal c (f b)
--rewrite a_b c_fa = let fa_fb = arg a_b in trans c_fa fa_fb

newtype Id x = Id { unId :: x }

-- | Coerce a type to another using an equality proof.
coerce :: Equal α β -> α -> β
coerce ab = subst Id unId ab
