-- | Reexports 'Prelude' from package "base"
-- hiding identifiers redefined by Copilot.

module Language.Copilot.Prelude
  ( module Prelude
  ) where

import Prelude hiding
  ( (++)
  , Eq
  , (==), (/=)
  , Ord
  , (<=), (>=), (<), (>)
  , (&&), (||)
  , (!!)
  , not
  , const
  , drop
  , mod
  )
