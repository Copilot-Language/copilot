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

data Format = Table | Sequence

-- | Interprets a Copilot specification.
interpret :: Format -> Int -> Env Name -> Spec -> String
interpret Table    k exts spec = renderAsTable    (eval k exts spec)
interpret Sequence k exts spec = renderAsSequence (eval k exts spec)
