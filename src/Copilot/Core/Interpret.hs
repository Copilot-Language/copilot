--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | An interpreter for Copilot specifications.

{-# LANGUAGE Safe #-}

module Copilot.Core.Interpret
  ( Format (..)
  , interpret
  ) where

import Copilot.Core
import Copilot.Core.Interpret.Eval
import Copilot.Core.Interpret.Render
import Copilot.Core.Type.Show (ShowType(..))

-- | Output format for the results of a Copilot spec interpretation.
data Format = Table | CSV

-- | Interpret a Copilot specification.
interpret :: Format  -- ^ Format to be used for the output.
          -> Int     -- ^ Number of steps to interpret.
          -> Spec    -- ^ Specification to interpret.
          -> String
interpret format k spec =
  case format of
    Table -> renderAsTable e
    CSV   -> renderAsCSV e
  where e = eval Haskell k spec
