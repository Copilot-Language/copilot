--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | An tagless interpreter for Copilot specifications.

{-# LANGUAGE Safe #-}

module Copilot.Core.Interpret.Render
  ( renderAsTable
  , renderAsCSV
  ) where

import Data.List (intersperse, transpose, foldl')
import Data.Maybe (catMaybes)
import Copilot.Core.Interpret.Eval (Output, ExecTrace (..))
import qualified Data.Map as M
import Text.PrettyPrint.NCol (asColumns)
import Text.PrettyPrint (Doc, ($$), (<>), text, render, empty)

--------------------------------------------------------------------------------

renderAsTable :: ExecTrace -> String
renderAsTable
  ExecTrace
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

renderAsCSV :: ExecTrace -> String
renderAsCSV = render . unfold

unfold :: ExecTrace -> Doc
unfold r =
  case step r of
    (cs, Nothing) -> cs
    (cs, Just r') -> cs $$ unfold r'

step :: ExecTrace -> (Doc, Maybe ExecTrace)
step ExecTrace
       { interpTriggers  = trigs
       } = 
  if M.null trigs then (empty, Nothing)
    else (foldl' ($$) empty (text "#" : ppTriggerOutputs), tails)

  where

  ppTriggerOutputs :: [Doc]
  ppTriggerOutputs =
      catMaybes 
    . fmap ppTriggerOutput 
    . M.assocs 
    . fmap head 
    $ trigs

  ppTriggerOutput :: (String, Maybe [Output]) -> Maybe Doc
  ppTriggerOutput (_,  Nothing) = Nothing
  ppTriggerOutput (cs, Just xs) = Just $
    text cs <> text "," <>
      (foldr (<>) empty . map text . intersperse ",") xs

  tails :: Maybe ExecTrace
  tails =
    if any null (M.elems (fmap tail trigs))
      then Nothing
      else Just
        ExecTrace
          { interpTriggers  = fmap tail trigs
          , interpObservers = M.empty
          }

--------------------------------------------------------------------------------
