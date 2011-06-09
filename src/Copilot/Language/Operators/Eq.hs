--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Copilot.Language.Operators.Eq
  ( Eq (..)
  ) where

import qualified Prelude as P
import Copilot.Language.Operators.Boolean
import Copilot.Language.Prelude

--------------------------------------------------------------------------------

class Boolean b => Eq a b where
  (==) :: a -> a -> b
  (/=) :: a -> a -> b
  x == y = not (x /= y)
  x /= y = not (x == y)

--------------------------------------------------------------------------------

instance P.Eq a => Eq a Bool where
  (==) = (P.==)
  (/=) = (P./=)

--------------------------------------------------------------------------------