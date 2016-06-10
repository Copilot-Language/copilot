{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE Safe #-}

module Copilot.Theorem.TransSys.Invariants
  ( HasInvariants (..)
  , prop
  ) where

class HasInvariants a where

  invariants :: a -> [(String, Bool)]

  checkInvs :: a -> Bool
  checkInvs obj = all snd $ invariants obj

prop :: String -> Bool -> (String, Bool)
prop = (,)
