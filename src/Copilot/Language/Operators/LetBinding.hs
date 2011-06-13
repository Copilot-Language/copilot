--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.LetBinding
  ( LetBinding (..)
  ) where

import Copilot.Core (Name, Typed)

--------------------------------------------------------------------------------

class LetBinding a where
  var :: (Show b, Typed b) => Name -> a b

--------------------------------------------------------------------------------
