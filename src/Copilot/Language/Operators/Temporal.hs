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
drop 0 s             = s
drop _ ( Const j )   = Const j
drop i ( Drop  j s ) = Drop (fromIntegral i + j) s
drop i s             = Drop (fromIntegral i)     s

--------------------------------------------------------------------------------