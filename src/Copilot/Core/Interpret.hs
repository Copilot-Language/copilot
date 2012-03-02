--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | An interpreter for Copilot specifications.

{-# LANGUAGE Safe #-}

module Copilot.Core.Interpret
  ( --ExtEnv (..)
    Format (..)
  , interpret
  ) where

import Copilot.Core
import Copilot.Core.Interpret.Eval
import Copilot.Core.Interpret.Render
import Copilot.Core.Type.Show (ShowType(..))

data Format = Table | CSV

-- | Interprets a Copilot specification.
interpret :: Format -> Int -> Spec -> String
interpret format k spec =
  case format of
    Table -> renderAsTable e
    CSV   -> renderAsCSV e
  where e = eval Haskell k spec
