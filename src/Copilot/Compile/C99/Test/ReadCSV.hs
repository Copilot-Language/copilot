--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.Test.ReadCSV (iterationsFromCSV) where

import Copilot.Core.Interpret.Eval (Output)
import Copilot.Core.Error (impossible)
import Copilot.Compile.C99.Test.Iteration (Iteration (..))

import Prelude as P
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Csv

parseError :: a
parseError = impossible "CSV parsing" "copilot-c99"

iterationsFromCSV :: ByteString -> [Iteration]
iterationsFromCSV bs =
  case decode NoHeader bs of
    Left _   -> parseError
    Right v  -> iterationsFromCSV' v

iterationsFromCSV' :: Csv -> [Iteration]
iterationsFromCSV' c = map Iteration (go M.empty c)
  where
  go :: M.Map String [Output] -> Csv -> [M.Map String [Output]]
  go m v
    | V.null v
    = [m]
    | nextIteration (V.head v)
    = m : go M.empty (V.tail v)
    | otherwise
    = let x  = V.head v in
      let m' = M.insert (triggerName x) (triggerOutputs x) m in
      go m' (V.tail v)

nextIteration :: V.Vector Field -> Bool
nextIteration v
  | V.length v == 1
  = B.unpack (V.head v) == "#"
  | otherwise
  = False

triggerName :: Record -> String
triggerName = B.unpack . V.head

triggerOutputs :: Record -> [Output]
triggerOutputs = V.toList . fmap B.unpack . V.tail
