-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | Show Copilot Core types and typed values.
module Copilot.Core.Type.Show
  {-# DEPRECATED "This module is deprecated in Copilot 3.10." #-}
  ( showWithType
  , ShowType(..)
  , showType
  ) where

import Copilot.Core.Type.ShowInternal (ShowType (..), showType, showWithType)
