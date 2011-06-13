--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Temporal
  ( (++)
  , drop
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Prelude
import Copilot.Language.Stream
import Prelude ()

--------------------------------------------------------------------------------

infixr 3 ++

(++) :: Typed a => [a] -> Stream a -> Stream a
(++) = (`Append` Nothing)

drop :: Typed a => Int -> Stream a -> Stream a
drop i = Drop (fromIntegral i)

--------------------------------------------------------------------------------