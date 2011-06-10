--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Copilot.Language.Operators.Integral
  ( Integral (..)
  ) where

import qualified Prelude as P

--------------------------------------------------------------------------------

class Integral a where
  div :: a -> a -> a
  mod :: a -> a -> a

--------------------------------------------------------------------------------

{-
instance P.Integral a => Integral a where
  div = P.div
  mod = P.mod
-}

--------------------------------------------------------------------------------