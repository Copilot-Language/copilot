{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE Safe #-}

-- | Augment types with invariants.

module Copilot.Theorem.TransSys.Invariants
  ( HasInvariants (..)
  , prop
  ) where

-- | Type class for types with additional invariants or constraints.
class HasInvariants a where

  invariants :: a -> [(String, Bool)]

  checkInvs :: a -> Bool
  checkInvs obj = all snd $ invariants obj

-- | Creates an invariant with a description.
prop :: String -> Bool -> (String, Bool)
prop = (,)
