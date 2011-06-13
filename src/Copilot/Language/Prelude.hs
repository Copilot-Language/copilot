--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Reexports 'Prelude' from package "base" hiding identifiers redefined by
-- Copilot.

module Copilot.Language.Prelude
  ( module Prelude
  ) where

import Prelude hiding
  ( (++)
  , (==), (/=)
  , div, mod
  , (<=), (>=), (<), (>)
  , (&&)
  , (||)
  , const
  , drop
  , not
  , mod )

--------------------------------------------------------------------------------