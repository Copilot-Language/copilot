module Main where

import Test.Hspec
import Test.QuickCheck

import Copilot.Compile.C99.Property.MatchesInterpreter
import Copilot.Compile.C99.Property.SequencePoint

main = hspec $ do
  describe "copilot-c99" $ do
    it "generates C99 code that matches the semantics of the interpreter." $ do
      property prop_matches_interpreter

    it "does not raise a sequence-point warning when updating the index." $ do
      prop_sequencepoint >>= shouldBe True
