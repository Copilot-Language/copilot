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
  , coerce2
  , refl
  , trans
  , symm
  , cong
  , cong2
  , cong3
  , cong4
  , subst
  --  , rewrite
  ) where

-- | An equality proof 'Equal' /a/ /b/ is a proof that /a/ is equal to /b/.
newtype Equal α β = Refl { unRefl :: forall f . f α -> f β }

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

coerce2 :: Equal α β -> f α -> f β
coerce2 (Refl f) = f

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

-- Lifting:
newtype Lift  f α β       = Lift  { unLift  :: Equal (f α)       (f β) }
newtype Lift2 f γ α β     = Lift2 { unlift2 :: Equal (f α γ)     (f β γ) }  
newtype Lift3 f γ δ α β   = Lift3 { unlift3 :: Equal (f α γ δ)   (f β γ δ) }
newtype Lift4 f ξ γ δ α β = Lift4 { unlift4 :: Equal (f α γ δ ξ) (f β γ δ ξ) }

lift :: Equal α β -> Equal (f α) (f β)
lift (Refl f) = unLift (f (Lift refl))

lift2 :: Equal α β  -> Equal (f α γ) (f β γ)
lift2 (Refl f) = unlift2 (f (Lift2 refl))

lift3 :: Equal α β -> Equal (f α γ δ) (f β γ δ)
lift3 (Refl f) = unlift3 (f (Lift3 refl))

lift4 :: Equal α β -> Equal (f α γ δ ξ) (f β γ δ ξ)
lift4 (Refl f) = unlift4 (f (Lift4 refl))

-- | Equality proofs are congruential.
cong
  :: Equal α1 α2
  -> Equal (f α1) (f α2)
cong = lift

cong2
  :: Equal α1 α2 -> Equal β1 β2
  -> Equal (f α1 β1) (f α2 β2)
cong2 p q = lift2 p `unRefl` lift q

cong3
  :: Equal α1 α2 -> Equal β1 β2 -> Equal γ1 γ2
  -> Equal (f α1 β1 γ1) (f α2 β2 γ2)
cong3 p q r = lift3 p `unRefl` lift2 q `unRefl` lift r

cong4
  :: Equal α1 α2 -> Equal β1 β2 -> Equal γ1 γ2 -> Equal δ1 δ2
  -> Equal (f α1 β1 γ1 δ1) (f α2 β2 γ2 δ2)
cong4 p q r w = lift4 p `unRefl` lift3 q `unRefl` lift2 r `unRefl` lift w

-- | Substitution using an equality proof.
--
-- Using the substitution rule we can use equality proofs to prove to the
-- Haskell type system that a type is an instance of a type-class:
--
-- @
-- data T a
--   = B (Equal a Bool)   -- booleans
--   | I (Equal a Int)    -- integers
--   | R (Equal a Double) -- reals
--
-- data NumWit a = Num a => NumWit -- witness
--
-- newtype WrapNumWit a = WrapNumWit { unWrapNumWit :: NumWit a }
--
-- numInst :: Num b => Equal a b -> NumInst a
-- numInst p = subst WrapNumInst unWrapNumInst (symm p) NumInst
--
-- numInstT :: T a -> Maybe (NumInst a)
-- numInstT (B _) = Nothing
-- numInstT (I p) = Just (numInst p)
-- numInstT (R p) = Just (numInst p)
--
-- add :: T a -> a -> a -> Maybe a
-- add t x y = case numInstT t of
--   Just NumInst -> Just (x + y)
--   Nothing      -> Nothing
-- @
subst :: (fα -> f α) -> (f β -> fβ) -> Equal α β -> fα -> fβ
subst from to (Refl f) = to . f . from

--rewrite :: Equal a b -> Equal c (f a) -> Equal c (f b)
--rewrite a_b c_fa = let fa_fb = arg a_b in trans c_fa fa_fb
