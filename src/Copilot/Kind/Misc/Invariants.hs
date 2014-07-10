{-# OPTIONS_GHC -O0 #-} 

module Copilot.Kind.Misc.Invariants
  ( HasInvariants (..)
  , prop
  ) where
  
import Data.List
import Control.Exception.Base (assert)
import Debug.Trace (trace)

class HasInvariants a where

  invariants :: a -> [(String, Bool)]
          
  checkInvs :: a -> Bool
  checkInvs obj = all id (map snd $ invariants obj)

          
prop :: String -> Bool -> (String, Bool)
prop = (,)