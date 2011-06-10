*Copyright © 2011 National Institute of Aerospace / Galois, Inc.*

`Copilot.Core.Type.Equality`
============================

> module Copilot.Core.Type.Equality
>  ( Equal
>  , EqualType (..)
>  , mkEqual
>  , coerce, coerce2
>  , refl, trans, symm
>  , cong, cong2, cong3, cong4
>  ) where

Overview
--------

This module implements leibnizian equality on types.

Resources
---------

The theory behind the technique is described the following paper:

  * Baars, Arthur I. and Swierstra, S. Doaitse, "*Typing dynamic typing*",
    ACM SIGPLAN Notices vol. 37, p. 157-166, 2002.

Equality Proofs
---------------

An equality proof `Equal a b` is a proof that `a` is equal to `b`:

> newtype Equal a b = Refl { unRefl :: forall f . f a -> f b }

A type class for constructing equality proofs:

> class EqualType τ where
>   (=~=) :: τ a -> τ b -> Maybe (Equal a b)

Leibnizian equality states that two things are equal if you can substite one for
the other in all contexts.

Constructor for building equality proofs:

> mkEqual :: (forall f . f a -> f b) -> Equal a b
> mkEqual = Refl

Reflexion, Symmetry, and Transitivity
-------------------------------------

Coerce a type to another using an equality proof:

> newtype Id x = Id { unId :: x }

> coerce :: Equal a b -> a -> b
> coerce (Refl f) = unId . f . Id

> coerce2 :: Equal a b -> f a -> f b
> coerce2 (Refl f) = f

Equality proofs are reflexive:

> refl :: Equal a a
> refl = Refl id

Equality proofs are symmetric:

> newtype Symm f a b = Symm { unSymm :: f b a }

> symm :: Equal a b -> Equal b a
> symm (Refl f) = unSymm (f (Symm refl))

Equality proofs are transitive:

> trans :: Equal a b -> Equal b c -> Equal a c
> trans (Refl f) (Refl g) = Refl (g . f)

Congruence
----------

We can lift equality proofs as follows:

> newtype Lift  f a b       = Lift  { unLift  :: Equal (f a)       (f b) }
> newtype Lift2 f c a b     = Lift2 { unlift2 :: Equal (f a c)     (f b c) }  
> newtype Lift3 f c d a b   = Lift3 { unlift3 :: Equal (f a c d)   (f b c d) }
> newtype Lift4 f e c d a b = Lift4 { unlift4 :: Equal (f a c d e) (f b c d e) }

> lift :: Equal a b -> Equal (f a) (f b)
> lift (Refl f) = unLift (f (Lift refl))

> lift2 :: Equal a b  -> Equal (f a c) (f b c)
> lift2 (Refl f) = unlift2 (f (Lift2 refl))

> lift3 :: Equal a b -> Equal (f a c d) (f b c d)
> lift3 (Refl f) = unlift3 (f (Lift3 refl))

> lift4 :: Equal α b -> Equal (f α c d ξ) (f b c d ξ)
> lift4 (Refl f) = unlift4 (f (Lift4 refl))

By using lifting we can define congruence as follows:

> cong :: Equal a1 a2
>      -> Equal (f a1) (f a2)
> cong = lift

> cong2
>   :: Equal a1 a2 -> Equal b1 b2
>  -> Equal (f a1 b1) (f a2 b2)
> cong2 p q = lift2 p `unRefl` lift q

> cong3
>   :: Equal a1 a2 -> Equal b1 b2 -> Equal c1 c2
>   -> Equal (f a1 b1 c1) (f a2 b2 c2)
> cong3 p q r = lift3 p `unRefl` lift2 q `unRefl` lift r

> cong4
>  :: Equal a1 a2 -> Equal b1 b2 -> Equal c1 c2 -> Equal d1 d2
>  -> Equal (f a1 b1 c1 d1) (f a2 b2 c2 d2)
> cong4 p q r w = lift4 p `unRefl` lift3 q `unRefl` lift2 r `unRefl` lift w

Examples
--------

By using witnesses we can prove to the Haskell type system at runtime that a
polymorphic type variable instantiates a type-class:

> data T a
>  = B (Equal a Bool)   -- booleans
>  | I (Equal a Int)    -- integers
>  | R (Equal a Double) -- reals

> data NumWit a = Num a => NumWit

> numWit :: T a -> Maybe (NumWit a)
> numWit (B _) = Nothing
> numWit (I p) = Just $ (`coerce2` NumWit) (symm p)
> numWit (R p) = Just $ (`coerce2` NumWit) (symm p)

> add :: T a -> a -> a -> Maybe a
> add t x y = case numWit t of
>   Just NumWit -> Just (x + y)
>   Nothing     -> Nothing
