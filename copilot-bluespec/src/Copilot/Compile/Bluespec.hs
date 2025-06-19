-- | Compile Copilot specifications to Bluespec code.
module Copilot.Compile.Bluespec
  ( compile
  , compileWith
  , BluespecSettings(..)
  , mkDefaultBluespecSettings
  ) where

-- Internal imports
import Copilot.Compile.Bluespec.Compile ( compile, compileWith )
import Copilot.Compile.Bluespec.Settings ( BluespecSettings (..),
                                           mkDefaultBluespecSettings )
