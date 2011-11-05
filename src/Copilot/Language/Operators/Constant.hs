--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Constants.

module Copilot.Language.Operators.Constant
  ( constant
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream

--------------------------------------------------------------------------------

constant :: Typed a => a -> Stream a
constant = Const

--------------------------------------------------------------------------------
