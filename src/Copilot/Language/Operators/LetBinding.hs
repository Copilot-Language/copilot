--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.LetBinding
  ( var
  ) where

import Copilot.Core (Name, Typed)
import Copilot.Language.Stream

--------------------------------------------------------------------------------

var :: Typed a => Name -> Stream a
var = LetBinding

--------------------------------------------------------------------------------