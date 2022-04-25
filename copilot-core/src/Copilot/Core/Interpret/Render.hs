-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | Pretty-print the results of a simulation.

{-# LANGUAGE Safe #-}

module Copilot.Core.Interpret.Render
  ( renderAsTable
  , renderAsCSV
  ) where

import Data.List (intersperse, transpose, foldl')
import Data.Maybe (catMaybes)
import Copilot.Core.Interpret.Eval (Output, ExecTrace (..))
import Text.PrettyPrint

import Prelude hiding ((<>))

-- | Render an execution trace as a table, formatted to faciliate readability.
renderAsTable :: ExecTrace -> String
renderAsTable
  ExecTrace
    { interpTriggers  = trigs
    , interpObservers = obsvs } = ( render
                                  . asColumns
                                  . transpose
                                  . (:) (ppTriggerNames ++ ppObserverNames)
                                  . transpose
                                  ) (ppTriggerOutputs ++ ppObserverOutputs)
     where

     ppTriggerNames :: [Doc]
     ppTriggerNames  = map (text . (++ ":")) (map fst trigs)

     ppObserverNames :: [Doc]
     ppObserverNames = map (text . (++ ":")) (map fst obsvs)

     ppTriggerOutputs :: [[Doc]]
     ppTriggerOutputs = map (map ppTriggerOutput) (map snd trigs)

     ppTriggerOutput :: Maybe [Output] -> Doc
     ppTriggerOutput (Just vs) = text $ "(" ++ concat (intersperse "," vs) ++ ")"
     ppTriggerOutput Nothing   = text "--"

     ppObserverOutputs :: [[Doc]]
     ppObserverOutputs = map (map text) (map snd obsvs)

-- | Render an execution trace as using comma-separate value (CSV) format.
renderAsCSV :: ExecTrace -> String
renderAsCSV = render . unfold

-- | Pretty print all the steps of the execution trace and concatenate the
-- results.
unfold :: ExecTrace -> Doc
unfold r =
  case step r of
    (cs, Nothing) -> cs
    (cs, Just r') -> cs $$ unfold r'

-- | Pretty print the state of the triggers, and provide a continuation
-- for the execution trace at the next point in time.
step :: ExecTrace -> (Doc, Maybe ExecTrace)
step ExecTrace
       { interpTriggers  = trigs
       } =
  if null trigs then (empty, Nothing)
    else (foldl' ($$) empty (text "#" : ppTriggerOutputs), tails)

  where

  ppTriggerOutputs :: [Doc]
  ppTriggerOutputs =
      catMaybes
    . fmap ppTriggerOutput
    . map (fmap head)
    $ trigs

  ppTriggerOutput :: (String, Maybe [Output]) -> Maybe Doc
  ppTriggerOutput (_,  Nothing) = Nothing
  ppTriggerOutput (cs, Just xs) = Just $
    text cs <> text "," <>
      (foldr (<>) empty . map text . intersperse ",") xs

  tails :: Maybe ExecTrace
  tails =
    if any null (fmap (tail.snd) trigs)
      then Nothing
      else Just
        ExecTrace
          { interpTriggers  = map (fmap tail) trigs
          , interpObservers = []
          }

-- Copied from pretty-ncols because of incompatibility with newer GHC versions.
asColumns :: [[Doc]] -> Doc
asColumns = flip asColumnsWithBuff $ 1

asColumnsWithBuff :: [[Doc]] -> Int -> Doc
asColumnsWithBuff lls q = normalize
        where normalize = vcat $ map hsep
                        $ map (\x -> pad (length x) longColumnLen empty x)
                        $ pad' longEntryLen q
                        $ transpose lls -- normalize column height
              longColumnLen = maximum (map length lls)
              longEntryLen = maximum $ map docLen (concat lls)

docLen d = length $ render d

-- | Pad a string on the right to reach an expected length.
pad :: Int -> Int -> a -> [a] -> [a]
pad lx max b ls = ls ++ replicate (max - lx) b

-- | Pad a list of strings on the right with spaces.
pad' :: Int      -- ^ Mininum number of spaces to add
     -> Int      -- ^ Maximum number of spaces to add
     -> [[Doc]]  -- ^ List of documents to pad
     -> [[Doc]]
pad' _ _ []       = []
pad' mx q (ls:xs) = map buf ls : pad' mx q xs
        where buf x = x <> (hcat $ replicate q space) <> (hcat $ replicate (mx - (docLen x)) space)
