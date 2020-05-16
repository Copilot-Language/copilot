module Main where

import Test.QuickCheck

import Copilot.Compile.C99.Property

main = quickCheck prop_matching_output
