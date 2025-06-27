-- |
-- Copyright : (c) NASA, 2024-2025
-- License   : BSD-style (see the LICENSE file in the distribution)
--
-- Produce a visual representation of a Copilot specification.
module Copilot.Visualize.Static
    ( visualize
    )
  where

-- External imports
import Data.Aeson             (ToJSON (..))
import System.Directory.Extra (copyTemplate)
import System.FilePath        ((</>))

-- External imports: Copilot
import Copilot.Core           (Spec (..))
import Copilot.Interpret.Eval (ShowType (Haskell), eval)

-- Internal imports
import Copilot.Visualize.UntypedTrace (makeTraceEval)
import Paths_copilot_visualizer       (getDataDir)

-- | Produce a visual representation of a Copilot specification, for a given
-- number of steps, using a template.
visualize :: Int      -- ^ Number of steps to interpret.
          -> Spec     -- ^ Specification to interpret.
          -> String   -- ^ Base used to expand the static file (i.e., @"tikz"@,
                      -- @"static_html"@).
          -> FilePath
          -> IO ()
visualize k spec base target = do
  dir <- getDataDir
  let f = dir </> "data" </> base
  let subs = toJSON $ makeTraceEval k spec $ eval Haskell k spec
  copyTemplate f subs target
