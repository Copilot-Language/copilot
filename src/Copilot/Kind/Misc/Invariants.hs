module Copilot.Kind.Misc.Invariants
  ( HasInvariants (..)
  , prop
  ) where
  
import Data.List

class HasInvariants a where

  invariants :: a -> [(String, Bool)]
  
  assertInvs :: forall b . a -> b -> b
  assertInvs obj x = foldl' f x (invariants obj)
    where f _ (s, False) = 
             error ("The following invariant was not respected :" ++ s)
          f acc _ = acc
          
  assertInvs_ :: (Monad m) => a -> m ()
  assertInvs_ obj = return $ assertInvs obj ()
          
  checkInvs :: a -> Bool
  checkInvs obj = all id (map snd $ invariants obj)

          
prop :: String -> Bool -> (String, Bool)
prop = (,)