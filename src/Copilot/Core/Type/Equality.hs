--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}

-- | Leibnizian equality on types.
-- The theory behind this technique is described the following paper:
--
-- * Baars, Arthur I. and Swierstra, S. Doaitse,
-- \"/Typing dynamic typing/\",
-- ACM SIGPLAN Notices vol. 37, p. 157-166, 2002

module Copilot.Core.Type.Equality
  ( Equal
  , EqualType (..)
  , mkEqual
  , coerce, coerce2
  , refl, trans, symm
  , cong, cong2, cong3, cong4
  , subst
  ) where

--------------------------------------------------------------------------------

-- | An equality proof 'Equal' /a/ /b/ is a proof that /a/ is equal to /b/.
newtype Equal a b = Refl { unRefl :: forall f . f a -> f b }

--------------------------------------------------------------------------------

-- | A type class for constructing equality proofs.
class EqualType t where
  (=~=) :: t a -> t b -> Maybe (Equal a b)

--------------------------------------------------------------------------------

-- | Constructor for building equality proofs.
-- Leibnizian equality states that two things are equal
-- if you can substite one for the other in all contexts.
mkEqual :: (forall f . f a -> f b) -> Equal a b
mkEqual = Refl

--------------------------------------------------------------------------------

newtype Id x = Id { unId :: x }
-- | Coerce a type to another using an equality proof.
coerce :: Equal a b -> a -> b
coerce (Refl f) = unId . f . Id

coerce2 :: Equal a b -> f a -> f b
coerce2 (Refl f) = f

--------------------------------------------------------------------------------

-- | Equality proofs are reflexive.
refl :: Equal a a
refl = Refl id

--------------------------------------------------------------------------------

-- | Equality proofs are transitive.
trans :: Equal a b -> Equal b c -> Equal a c
trans (Refl f) (Refl g) = Refl (g . f)

--------------------------------------------------------------------------------

newtype Symm f a b = Symm { unSymm :: f b a }
-- | Equality proofs are symmetric.
symm :: Equal a b -> Equal b a
symm (Refl f) = unSymm (f (Symm refl))

--------------------------------------------------------------------------------

-- Lifting:
newtype Lift  f a b       = Lift  { unLift  :: Equal (f a)       (f b) }
newtype Lift2 f c a b     = Lift2 { unlift2 :: Equal (f a c)     (f b c) }  
newtype Lift3 f c d a b   = Lift3 { unlift3 :: Equal (f a c d)   (f b c d) }
newtype Lift4 f ξ c d a b = Lift4 { unlift4 :: Equal (f a c d ξ) (f b c d ξ) }

lift :: Equal a b -> Equal (f a) (f b)
lift (Refl f) = unLift (f (Lift refl))

lift2 :: Equal a b  -> Equal (f a c) (f b c)
lift2 (Refl f) = unlift2 (f (Lift2 refl))

lift3 :: Equal a b -> Equal (f a c d) (f b c d)
lift3 (Refl f) = unlift3 (f (Lift3 refl))

lift4 :: Equal a b -> Equal (f a c d ξ) (f b c d ξ)
lift4 (Refl f) = unlift4 (f (Lift4 refl))

--------------------------------------------------------------------------------

-- | Equality proofs are congruential.
cong
  :: Equal a1 a2
  -> Equal (f a1) (f a2)
cong = lift

cong2
  :: Equal a1 a2 -> Equal b1 b2
  -> Equal (f a1 b1) (f a2 b2)
cong2 p q = lift2 p `unRefl` lift q

cong3
  :: Equal a1 a2 -> Equal b1 b2 -> Equal c1 c2
  -> Equal (f a1 b1 c1) (f a2 b2 c2)
cong3 p q r = lift3 p `unRefl` lift2 q `unRefl` lift r

cong4
  :: Equal a1 a2 -> Equal b1 b2 -> Equal c1 c2 -> Equal d1 d2
  -> Equal (f a1 b1 c1 d1) (f a2 b2 c2 d2)
cong4 p q r w = lift4 p `unRefl` lift3 q `unRefl` lift2 r `unRefl` lift w

--------------------------------------------------------------------------------

-- | Substitution using an equality proof.
--
-- By using substitution we can prove to the Haskell type system at runtime
-- that a polymorphic type variable instantiates a type-class:
--
-- @
-- data T a
--   = B (Equal a Bool)   -- booleans
--   | I (Equal a Int)    -- integers
--   | R (Equal a Double) -- reals
--
-- data NumWit a = Num a => NumWit -- A witness that /a/ instantiates 'Num'.
--
-- mkNumWit :: U b => Equal a b -> NumWit a
-- mkNumWit = (`coerce` NumWit) . cong . symm
--
-- numWitT :: T a -> Maybe (NumWit a)
-- numWitT (B _) = Nothing
-- numWitT (I p) = Just (mkNumWit p)
-- numWitT (R p) = Just (mkNumWit p)
--
-- add :: T a -> a -> a -> Maybe a
-- add t x y = case numWitT t of
--   Just NumWit -> Just (x + y)
--   Nothing     -> Nothing
-- @
subst :: (fa -> f a) -> (f b -> fb) -> Equal a b -> fa -> fb
subst from to (Refl f) = to . f . from

--------------------------------------------------------------------------------
