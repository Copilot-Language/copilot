--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Let expressions.

{-# LANGUAGE Trustworthy #-}

module Copilot.Language.Operators.Local
  ( local
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream (Stream (..))

--------------------------------------------------------------------------------

local :: (Typed a, Typed b) => Stream a -> (Stream a -> Stream b) -> Stream b
local = Local

--------------------------------------------------------------------------------
