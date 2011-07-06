--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.Test.Iteration
  ( Iteration (..)
  , execTraceToIterations
  ) where

import Copilot.Core.Interpret.Eval (ExecTrace (..), Output)
import Data.Function (on)
import Data.List (unfoldr)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)

-- An iteration represents the output of all triggers within a single
-- iteration.
newtype Iteration =
  Iteration
    { iterationOutputs :: (Map String [Output]) }
  deriving (Show, Eq)

execTraceToIterations :: ExecTrace -> [Iteration]
execTraceToIterations = unfoldr step

  where

  step :: ExecTrace -> Maybe (Iteration, ExecTrace)
  step trace@ExecTrace { interpTriggers  = trigs }

    | nullary   = Nothing

    | otherwise = Just (iteration, tails)

    where

    iteration :: Iteration
    iteration = Iteration $
      fmap fromJust
        $ M.filter isJust
        $ fmap head
        $ trigs

    nullary :: Bool
    nullary = any null (M.elems trigs)

    tails :: ExecTrace
    tails = trace { interpTriggers = fmap tail trigs }
