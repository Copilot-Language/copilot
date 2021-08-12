-- | Compile Copilot specifications to C99 code.
module Copilot.Compile.C99
  ( compile
  , compileWith
  , CSettings(..)
  , mkDefaultCSettings
  ) where

import Copilot.Compile.C99.Compile
