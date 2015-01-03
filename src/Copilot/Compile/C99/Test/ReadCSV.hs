--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.Test.ReadCSV (iterationsFromCSV) where

import Copilot.Core.Interpret.Eval (Output)
import Copilot.Core.Error (impossible)
import Copilot.Compile.C99.Test.Iteration (Iteration (..))

import Prelude as P
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Text.CSV as C

parseError :: a
parseError = impossible "CSV parsing" "copilot-c99"

iterationsFromCSV :: B.ByteString -> [Iteration]
iterationsFromCSV bs =
  case C.parseCSV "csvParseErrors" (B.unpack bs) of
    Left  err -> error (show err)
    Right res -> iterationsFromCSV' res

iterationsFromCSV' :: C.CSV -> [Iteration]
iterationsFromCSV' = map Iteration . go M.empty
  where
  go m []             = [m]
  go m (x:xs)
    | nextIteration x = m : go M.empty xs
    | otherwise =
        let
          m' = M.insert (triggerName x) (triggerOutputs x) m
        in
          go m' xs

nextIteration :: Record -> Bool
nextIteration [x] = x == "#"
nextIteration _   = False

triggerName :: Record -> String
triggerName = head

triggerOutputs :: Record -> [Output]
triggerOutputs = tail

