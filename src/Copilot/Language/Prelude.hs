-- | Reexports 'Prelude' from package "base"
-- hiding identifiers redefined by Copilot.

module Copilot.Language.Prelude
  ( module Prelude
  ) where

import Prelude hiding
  ( (++)
  , Eq (..)
  , Ord (..)
  , (&&)
  , (||)
  , const
  , drop
  , not
  , mod
  )
