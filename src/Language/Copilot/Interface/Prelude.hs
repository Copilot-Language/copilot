-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- | Reexports 'Prelude' from package "base"
-- hiding identifiers redefined by Copilot.

module Language.Copilot.Interface.Prelude
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
