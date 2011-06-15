--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Local
  ( local
  , var
  ) where

import Copilot.Core (Name, Typed)
import Copilot.Language.Stream

--------------------------------------------------------------------------------

local :: (Typed a, Typed b) => Name -> Stream a -> Stream b -> Stream b
local = Local

var :: Typed a => Name -> Stream a
var = Var

--------------------------------------------------------------------------------