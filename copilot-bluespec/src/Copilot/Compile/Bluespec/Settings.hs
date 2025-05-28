-- | Settings used by the code generator to customize the code.
module Copilot.Compile.Bluespec.Settings
  ( BluespecSettings(..)
  , mkDefaultBluespecSettings
  ) where

-- | Settings used to customize the code generated.
newtype BluespecSettings = BluespecSettings
  { bluespecSettingsOutputDirectory :: FilePath
  }

-- | Default Bluespec settings. Output to the current directory.
mkDefaultBluespecSettings :: BluespecSettings
mkDefaultBluespecSettings = BluespecSettings "."
