module Main where

import Test.QuickCheck

import Copilot.Compile.C99.Property.MatchesInterpreter

main = quickCheck prop_matches_interpreter
