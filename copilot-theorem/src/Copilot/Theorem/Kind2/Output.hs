{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe       #-}

-- | Parse output of Kind2.
module Copilot.Theorem.Kind2.Output (parseOutput) where

import Data.List.Extra (isInfixOf, splitOn, stripInfix, trim)
import Data.Maybe      (mapMaybe)

import Copilot.Theorem.Prove as P

import qualified Copilot.Core as C

import qualified Copilot.Theorem.Misc.Error as Err

-- | Parse output of Kind2.
--
-- The output is expected to be in XML format, as produced by Kind2's @-xml@
-- flag and documented at
-- <https://kind.cs.uiowa.edu/kind2_user_doc/3_output/2_machine_readable.html>.
-- Note that this function does not parse the output as XML: when given a
-- system in the native transition system format, Kind2 produces output that
-- is not always well-formed XML (e.g., counterexamples to invariant
-- properties cannot be printed for systems in that format -- the
-- counterexample printing functions in Kind2's @src/inputSystem.ml@ fail with
-- an internal error for native input -- and Kind2 reports the error in the
-- middle of the XML output). Instead, this function looks for the
-- @\<Property\>@ tag for the given property and extracts the answers in the
-- @\<Answer\>@ tags it contains.
parseOutput :: String    -- ^ Property whose validity is being checked.
            -> C.Prop    -- ^ The property's quantifier.
            -> String    -- ^ XML output of Kind2
            -> P.Output
parseOutput propId propQuantifier xml
    | "valid"       `elem` answers = quantified (Output Valid   [])
                                                (Output Invalid [])
    | "falsifiable" `elem` answers = quantified (Output Invalid [])
                                                (Output Valid   [])
    | "unknown"     `elem` answers = Output Unknown []
    | null answers = err $ "Answer for property " ++ propId ++ " not found"
    | otherwise    = err $ "Unrecognized status : " ++ unwords answers

  where

    -- Pick an output based on the quantifier of the property. The first
    -- argument is returned when the property Kind2 was asked about is valid,
    -- and the second one when it is invalid.
    --
    -- We encode a universally quantified property P as ∀x.P(x) in Kind2, so
    -- the original property is valid iff the Kind2 property is valid.
    --
    -- We encode an existentially quantified property P as ¬(∀x.¬(P(x))) in
    -- Kind2, so the original property is valid iff the Kind2 property is
    -- invalid.
    quantified ifValid ifInvalid = case propQuantifier of
      C.Forall {} -> ifValid
      C.Exists {} -> ifInvalid

    -- All the answers reported for the property, in the order in which they
    -- appear in the output. A conclusive answer (valid or falsifiable), if
    -- any, takes precedence over inconclusive (unknown) ones, which Kind2 may
    -- also report when the analysis terminates without an answer for all
    -- properties.
    answers = mapMaybe answerText
            $ filter isRightProperty
            $ propertyElems xml

    -- Substrings of the output following a @\<Property @ opening tag, each
    -- extending to the next such tag (or to the end of the output).
    propertyElems = drop 1 . splitOn "<Property "

    -- True if the given property element has the name of the property this
    -- function looks for.
    isRightProperty elem' =
      ("name=\"" ++ escapeAttr propId ++ "\"") `isInfixOf` openingTag
      where
        openingTag = takeWhile (/= '>') elem'

    -- The contents of the first @\<Answer\>@ tag in the given element, if
    -- any.
    answerText elem' = do
      (_, rest)  <- stripInfix "<Answer" elem'
      (_, rest') <- stripInfix ">" rest
      let answer = trim $ takeWhile (/= '<') rest'
      if null answer then Nothing else Just answer

    err :: forall a . String -> a
    err msg = Err.fatal $
      "Parse error while reading the Kind2 XML output : \n"
      ++ msg ++ "\n\n" ++ xml

-- | Escape a string for use as an XML attribute value.
escapeAttr :: String -> String
escapeAttr = concatMap escapeChar
  where
    escapeChar '&' = "&amp;"
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '"' = "&quot;"
    escapeChar c   = [c]
