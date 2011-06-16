--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Core.Random.Gen
  ( Gen
  , runGen
  , randomFromType
  , oneOf
  , freq
  , choose
  , elements
  , depth
  , weights
  , incDepth
  ) where

import Copilot.Core.Random.Weights
import Copilot.Core.Type
import Copilot.Core.Type.Equality
import System.Random (StdGen, Random, random, randomR, split)

--------------------------------------------------------------------------------

newtype Gen a = MkGen { runGen :: Depth -> Weights -> StdGen -> a }

--------------------------------------------------------------------------------

instance Functor Gen where
  fmap f (MkGen h) = MkGen (\ d ws r -> f (h d ws r))

instance Monad Gen where
  return x = MkGen (\ _ _ _ -> x)
  MkGen m >>= k = MkGen $ \ d ws r ->
    let
      (r1, r2) = split r
      MkGen m' = k (m d ws r1)
     in
      m' d ws r2

--------------------------------------------------------------------------------

stdGen :: Gen StdGen
stdGen = MkGen $ \ _ _ g -> g

depth :: Gen Depth
depth = MkGen $ \ d _ _ -> d

weights :: Gen Weights
weights = MkGen $ \ _ ws _ -> ws

incDepth :: Gen a -> Gen a
incDepth gen = MkGen $ \ d ws g -> runGen gen (succ d) ws g

--------------------------------------------------------------------------------

randomFromType :: Type a -> Gen a
randomFromType t =
  case t of
    Bool   p -> coerce (cong (symm p)) genBool
    Int8   p -> coerce (cong (symm p)) genBoundedIntegral
    Int16  p -> coerce (cong (symm p)) genBoundedIntegral
    Int32  p -> coerce (cong (symm p)) genBoundedIntegral
    Int64  p -> coerce (cong (symm p)) genBoundedIntegral
    Word8  p -> coerce (cong (symm p)) genBoundedIntegral
    Word16 p -> coerce (cong (symm p)) genBoundedIntegral
    Word32 p -> coerce (cong (symm p)) genBoundedIntegral
    Word64 p -> coerce (cong (symm p)) genBoundedIntegral
    Float  p -> coerce (cong (symm p)) genFractional
    Double p -> coerce (cong (symm p)) genFractional

  where

  genBool :: Gen Bool
  genBool =
    do
      g <- stdGen
      return $ fst (random g)

  genBoundedIntegral :: (Bounded a, Integral a) => Gen a
  genBoundedIntegral =
    do let mn = minBound
           mx = maxBound `asTypeOf` mn
       n <- choose (toInteger mn, toInteger mx)
       return (fromInteger n `asTypeOf` mn)

  genFractional :: (Random a, Fractional a) => Gen a
  genFractional =
    do
      g <- stdGen
      return $ fst (random g)

--------------------------------------------------------------------------------

choose :: Random a => (a, a) -> Gen a
choose rng =
  do
    g <- stdGen
    return $ fst (randomR rng g)

oneOf :: [Gen a] -> Gen a
oneOf [] = error "Copilot.Core.Spec.Random.Gen.oneof used with empty list"
oneOf gs = choose (0,length gs - 1) >>= (gs !!)

freq :: [(Int, Gen a)] -> Gen a
freq [] = error "Copilot.Core.Spec.Random.Gen.freq used with empty list"
freq xs0 = choose (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "Copilot.Core.Spec.Random.Gen.pick used with empty list"

elements :: [a] -> Gen a
elements [] = error "Copilot.Core.Spec.Random.Gen.elements used with empty list"
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

--------------------------------------------------------------------------------