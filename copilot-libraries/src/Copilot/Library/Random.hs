{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Basic pseudo-random number generation.
--
-- Information on performance of a selection of pseudo-random number generation
-- algorithms, including those implemented below, can be found at:
-- https://prng.di.unimi.it/
--
-- Important: these pseudo-random number generators are NOT cryptographically
-- secure generators.
module Copilot.Library.Random
    ( randomW32
    , randomW64
    , boundedRandomNumber
    , proportionalRandomBool
    , weightedRandomSelection
    )
  where

-- External imports
import Copilot.Language (Array, Stream, Typed, Word32, Word64, arrayElems,
                         constant, mod, mux, (!), (++), (.<<.), (.>>.), (.^.),
                         (<), (<=))
import GHC.TypeLits     (KnownNat, type (<=))
import Prelude          hiding (mod, (++), (<), (<=))

-- | Given two Word32 seeds, produce a pseudo-random stream of Word32 numbers,
-- which has a period of 2^32 - 1. Neither of the seeds should be zero, since
-- the underlying randomization algorithm maps zero to itself.
--
-- This function uses an Xorshift+ algorithm.
randomW32 :: Word32 -> Word32 -> Stream Word32
randomW32 0     _     = error "randomW32: seed cannot be zero."
randomW32 _     0     = error "randomW32: seed cannot be zero."
randomW32 seed1 seed2 = s1 + s2

  where

    s1 :: Stream Word32
    s1 = [seed1] ++ (xorshift32 s1)

    s2 :: Stream Word32
    s2 = [seed2] ++ (xorshift32 s2)

    -- This is a simple implementation of an Xorshift algorithm
    xorshift32 :: Stream Word32 -> Stream Word32
    xorshift32 x1 = x4  -- Note: 0 maps to itself with this
      where
        x4 = x3 .^. (x3 .<<. (5  :: Stream Word32))
        x3 = x2 .^. (x2 .>>. (17 :: Stream Word32))
        x2 = x1 .^. (x1 .<<. (13 :: Stream Word32))

-- | Given two Word64 seeds, produce a pseudo-random stream of Word64 numbers,
-- which has a period of 2^128 - 1. Neither of the seeds should be zero, as
-- doing so will negatively impact the quality of the randomness.
--
-- This function uses an Xorshiftrplus algorithm.
randomW64 :: Word64 -> Word64 -> Stream Word64
randomW64 0     _     = error "randomW64: seed cannot be zero."
randomW64 _     0     = error "randomW64: seed cannot be zero."
randomW64 seed1 seed2 = x

  where

    s0 :: Stream Word64
    s0 = [seed1] ++ s1

    s1 :: Stream Word64
    s1 = [seed2] ++ (x + s1)

    x :: Stream Word64
    x = xorshiftr128plus s0 s1

    xorshiftr128plus :: Stream Word64 -> Stream Word64 -> Stream Word64
    xorshiftr128plus x1 y = x4
      where
        x4 = x3 .^. y
        x3 = x2 .^. (x2 .>>. (17 :: Stream Word64))
        x2 = x1 .^. (x1 .<<. (23 :: Stream Word64))

-- | Given a stream of random integral numbers, a minimum value, and a maximum
-- value, produce a stream of random values greater than or equal to the
-- minimum and less than the maximum value, i.e., [@mN@, @mX@).
boundedRandomIntegral :: (Typed a, Integral a)
                      => Stream a -> a -> a -> Stream a
boundedRandomIntegral gen mN mX = (gen `mod` constant (mX - mN)) + constant mN

-- | Given a stream of random integral numbers and two integers defining the
-- numerator and denominator of a rational probability, produce a boolean
-- stream that is randomly @True@ with the given probability. That is to say,
-- given a numerator @n@ and denominator @d@, then, over a sufficiently large
-- sample size, the resulting stream @s@ is @True@ @n@ out of @d@ times.
proportionalRandomBool :: (Typed a, Integral a)
                       => Stream a -> a -> a -> Stream Bool
proportionalRandomBool gen n d = (gen `mod` constant d) < constant n

-- | Given a stream of random integral numbers, an array of values, and an
-- equal length array of weights, produce a stream of random values from the
-- given array with probabilities proportional to the given weights.
weightedRandomSelection :: forall a b s
                        .  (KnownNat s, s <= MaxWord32, Typed a, Typed b,
                            Integral a, Bounded a)
                        => Stream a -> Array s b -> Array s a -> Stream b
weightedRandomSelection gen values weights
    | overflow
    = error "weightedRandomSelection: sum of weights overflows type"

    | signedType
    = error "weightedRandomSelection: type of weights array must be unsigned"

    | otherwise
    = constant values ! idx

  where

    -- Check if cumulativeSum decreases at any point. For unsigned integral
    -- types, a decreasing element in this cumulative sequence corresponds to
    -- an overflow.
    overflow = or (zipWith (>) cumulativeSum (drop 1 cumulativeSum))

    signedType = 0 > (minBound :: a)

    idx :: Stream Word32
    idx = countLeq cumulativeSum (gen `mod` constant totalSum)

    countLeq :: [a] -> Stream a -> Stream Word32
    countLeq xs v = sum [ mux (constant x <= v) 1 0 | x <- xs ]
    -- ^ get number of elements in @xs@ less than or equal to @v@.
    -- For non-decreasing @xs@, this is equal to the index of the first
    -- element greater than @v@.

    cumulativeSum :: [a]
    cumulativeSum = scanl1 (+) (arrayElems weights)

    totalSum :: a
    totalSum = last cumulativeSum

-- | Type-level representation of the upper bound on an unsigned 32-bit
-- integer.
type MaxWord32 = 4294967295 -- 2^32 - 1
