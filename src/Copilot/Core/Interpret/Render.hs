--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | An tagless interpreter for Copilot specifications.

module Copilot.Core.Interpret.Render
  ( renderAsTable
  , renderAsSequence
  ) where

import Data.List (intersperse, transpose)
import Data.Maybe (catMaybes)
import Copilot.Core.Interpret.Eval (Output, Result (..))
import qualified Data.Map as M
import Text.PrettyPrint.NCol (asColumns)
import Text.PrettyPrint (Doc, ($$), (<+>), text, render, empty)

--------------------------------------------------------------------------------

renderAsTable :: Result -> String
renderAsTable
  Result
    { interpTriggers  = trigs
    , interpObservers = obsvs } =
  ( render
  . asColumns
  . transpose
  . (:) (ppTriggerNames ++ ppObserverNames)
  . transpose
  ) (ppTriggerOutputs ++ ppObserverOutputs)

  where

  ppTriggerNames :: [Doc]
  ppTriggerNames  = map (text . (++ ":")) (M.keys trigs)

  ppObserverNames :: [Doc]
  ppObserverNames = map (text . (++ ":")) (M.keys obsvs)

  ppTriggerOutputs :: [[Doc]]
  ppTriggerOutputs = map (map ppTriggerOutput) (M.elems trigs)

  ppTriggerOutput :: Maybe [Output] -> Doc
  ppTriggerOutput (Just vs) = text $ "(" ++ concat (intersperse "," vs) ++ ")"
  ppTriggerOutput Nothing   = text "--"

  ppObserverOutputs :: [[Doc]]
  ppObserverOutputs = map (map text) (M.elems obsvs)

--------------------------------------------------------------------------------

renderAsSequence :: Result -> String
renderAsSequence = render . unfold

unfold :: Result -> Doc
unfold r =
  case step r of
    (cs, Nothing) -> cs
    (cs, Just r') -> cs $$ unfold r'

step :: Result -> (Doc, Maybe Result)
step
  Result
    { interpTriggers  = trigs
    , interpObservers = obsvs
    } = (foldr ($$) empty (text "-- Iteration --" : ppTriggerOutputs), tails)

  where

  ppTriggerOutputs :: [Doc]
  ppTriggerOutputs =
    catMaybes . fmap ppTriggerOutput . M.assocs . fmap head $ trigs

  ppTriggerOutput :: (String, Maybe [Output]) -> Maybe Doc
  ppTriggerOutput (_,  Nothing) = Nothing
  ppTriggerOutput (cs, Just xs) = Just $
    text cs <+> text " : " <+>
      (foldr (<+>) empty . map text . intersperse ",") xs

  tails :: Maybe Result
  tails =
    if any null (M.elems (fmap tail trigs))
      then Nothing
      else Just
        Result
          { interpTriggers  = fmap tail trigs
          , interpObservers = obsvs }

--------------------------------------------------------------------------------