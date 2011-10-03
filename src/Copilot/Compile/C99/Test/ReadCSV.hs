--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.Test.ReadCSV (iterationsFromCSV) where

import Copilot.Core.Interpret.Eval (Output)
import Copilot.Compile.C99.Test.Iteration (Iteration (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Prelude as P
import Text.CSV.ByteString

parseError :: a
parseError = error "Utilities.Parser.readCSV: parse error!"

iterationsFromCSV :: ByteString -> [Iteration]
iterationsFromCSV = iterationsFromCSV' . handleMaybe . parseCSV

handleMaybe :: Maybe a -> a
handleMaybe (Just x) = x
handleMaybe Nothing  = parseError

iterationsFromCSV' :: CSV -> [Iteration]
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
nextIteration [x] = B.unpack x == "#"
nextIteration _   = False

triggerName :: Record -> String
triggerName = B.unpack . head

triggerOutputs :: Record -> [Output]
triggerOutputs = fmap B.unpack . tail

{-

readCSV :: ByteString -> ExecTrace
readCSV = execTraceFromCSV . handleMaybe . parseCSV

execTraceFromCSV :: CSV -> ExecTrace
execTraceFromCSV = execTraceFromIterations . iterationsFromCSV

execTraceFromIterations :: [Iteration] -> ExecTrace
execTraceFromIterations is =
  ExecTrace
    { interpTriggers  = triggerOutputs
    , interpObservers = M.empty }

  where

  triggerNames :: [String]
  triggerNames = nub $ concat $ fmap M.keys $ map iterationOutputs $ is

  initialTriggerOutputs :: Map String [Maybe [Output]]
  initialTriggerOutputs = M.fromList $ zip triggerNames (repeat [])

  triggerOutputs :: Map String [Maybe [Output]]
  triggerOutputs = fmap reverse $ go initialTriggerOutputs is

    where

    go m []     = m
    go m (i:is) = go (M.mapWithKey step m) is

      where

      step cs xs = M.lookup cs i : xs
-}
