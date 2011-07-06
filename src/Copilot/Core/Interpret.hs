--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | An interpreter for Copilot specifications.

module Copilot.Core.Interpret
  ( Env
  , Format (..)
  , interpret
  ) where

import Copilot.Core
import Copilot.Core.Interpret.Eval
import Copilot.Core.Interpret.Render

data Format = Table | CSV

-- | Interprets a Copilot specification.
interpret :: Format -> Int -> Env Name -> Spec -> String
interpret Table k exts spec = renderAsTable (eval k exts spec)
interpret CSV   k exts spec = renderAsCSV   (eval k exts spec)
