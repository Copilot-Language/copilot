--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Let expressions.

{-# LANGUAGE Trustworthy #-}

module Copilot.Language.Operators.Label
  ( label
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream (Stream (..))

--------------------------------------------------------------------------------

label :: (Typed a) => String -> Stream a -> Stream a
label s e = Op2 (Core.Label typeOf) s e

--------------------------------------------------------------------------------
