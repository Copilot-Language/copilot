-- | Compile Copilot specifications to C99 code.
module Copilot.Compile.C99
  ( compile
  , compileWith
  , CSettings(..)
  , mkDefaultCSettings
  ) where

-- Internal imports
import Copilot.Compile.C99.Compile  ( compile, compileWith )
import Copilot.Compile.C99.Settings ( CSettings (..), mkDefaultCSettings )
