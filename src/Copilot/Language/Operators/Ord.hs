--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Copilot.Language.Operators.Ord
  ( Ord (..)
  ) where

import Copilot.Language.Operators.Eq
import Prelude ()

--------------------------------------------------------------------------------

class Eq a b => Ord a b where
  (<=) :: a -> a -> b
  (>=) :: a -> a -> b
  (<)  :: a -> a -> b
  (>)  :: a -> a -> b

--------------------------------------------------------------------------------