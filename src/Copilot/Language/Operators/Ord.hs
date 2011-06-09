--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Copilot.Language.Operators.Ord
  ( Ord (..)
  ) where

import qualified Prelude as P
import Copilot.Language.Operators.Boolean
import Copilot.Language.Operators.Eq
import Copilot.Language.Prelude

--------------------------------------------------------------------------------

class (Boolean b, Eq a b) => Ord a b where
  (<=) :: a -> a -> b
  (>=) :: a -> a -> b
  (<)  :: a -> a -> b
  (>)  :: a -> a -> b

--------------------------------------------------------------------------------

instance P.Ord a => Ord a Bool where
  (<=) = (P.<=)
  (>=) = (P.>=)
  (<)  = (P.<)
  (>)  = (P.>)

--------------------------------------------------------------------------------