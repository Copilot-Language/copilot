--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Extern
  ( extern
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream

--------------------------------------------------------------------------------

extern :: Typed a => String -> Stream a
extern = Extern

--------------------------------------------------------------------------------