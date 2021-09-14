-- | Compile Copilot specifications to C99 code.
module Copilot.Compile.C99.Compile
  {-# DEPRECATED "This module will be hidden in future versions." #-}
  ( compile
  , compileWith
  , CSettings(..)
  , mkDefaultCSettings
  ) where

import Copilot.Compile.C99.Compile.Internal
import Copilot.Compile.C99.Settings.Internal
