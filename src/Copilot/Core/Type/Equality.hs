-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | Leibnizian equality on types.
-- The theory behind this technique is described the following paper:
--
-- * Baars, Arthur I. and Swierstra, S. Doaitse,
-- \"/Typing dynamic typing/\",
-- ACM SIGPLAN Notices vol. 37, p. 157-166, 2002

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Type.Equality
  ( Equal
  , EqualType (..)
  , mkEqual
  , coerce
  , refl
  , trans
  , symm
  , cong
  , subst
  --  , rewrite
  ) where

-- | An equality proof 'Equal' /a/ /b/ is a proof that /a/ is equal to /b/.
newtype Equal α β = Refl (forall f . f α -> f β)

-- | A type class for constructing equality proofs.
class EqualType τ where
  (=~=) :: τ α -> τ β -> Maybe (Equal α β)

-- | Constructor for building equality proofs.
-- Leibnizian equality states that two things are equal
-- if you can substite one for the other in all contexts.
mkEqual :: (forall f . f α -> f β) -> Equal α β
mkEqual = Refl

newtype Id x = Id { unId :: x }
-- | Coerce a type to another using an equality proof.
coerce :: Equal α β -> α -> β
coerce (Refl f) = unId . f . Id

-- | Equality proofs are reflexive.
refl :: Equal α α
refl = Refl id

-- | Equality proofs are transitive.
trans :: Equal α β -> Equal β γ -> Equal α γ
trans (Refl f) (Refl g) = Refl (g . f)

newtype Symm f α β = Symm { unSymm :: f β α }
-- | Equality proofs are symmetric.
symm :: Equal α β -> Equal β α
symm (Refl f) = unSymm (f (Symm refl))

newtype Lift f α β = Lift { unLift :: Equal (f α) (f β) }
-- | Equality proofs are congruential.
cong :: Equal α β -> Equal (f α) (f β)
cong (Refl f) = unLift (f (Lift refl))

-- | Substitution using an equality proof.
--
-- Using the substitution rule we can use equality proofs to prove to the
-- Haskell type system that a type is an instance of a type-class:
--
-- @
--     data T a
--       = B (Equal a Bool)   -- booleans
--       | I (Equal a Int)    -- integers
--       | R (Equal a Double) -- reals
--
--     data NumWit a = Num a => NumWit -- witness
--
--     newtype WrapNumWit a = WrapNumWit { unWrapNumWit :: NumWit a }
--
--     numInst :: Num b => Equal a b -> NumInst a
--     numInst p = subst WrapNumInst unWrapNumInst (symm p) NumInst
--
--     numInstT :: T a -> Maybe (NumInst a)
--     numInstT (B _) = Nothing
--     numInstT (I p) = Just (numInst p)
--     numInstT (R p) = Just (numInst p)
--
--     add :: T a -> a -> a -> Maybe a
--     add t x y = case numInstT t of
--       Just NumInst -> Just (x + y)
--       Nothing      -> Nothing
-- @
subst :: (fα -> f α) -> (f β -> fβ) -> Equal α β -> fα -> fβ
subst from to (Refl f) = to . f . from

--rewrite :: Equal a b -> Equal c (f a) -> Equal c (f b)
--rewrite a_b c_fa = let fa_fb = arg a_b in trans c_fa fa_fb
