--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Main (main) where

import qualified Copilot.Core as Core
import Copilot.Language.Reify
import Copilot.Core.Interpret
import Copilot.Core.PrettyPrint

import qualified Copilot.Kind.TransSys    as TS
import qualified Copilot.Kind.Kind2
import Copilot.Kind.Kind2.Prover
import Copilot.Kind.Naive
import Copilot.Kind.Prover

import Grey

--------------------------------------------------------------------------------

line = replicate 40 '-'

prover = naiveProver def `combine` kind2Prover def
--prover = kind2Prover (def {bmcMax = 20})


main :: IO ()
main =  do
  cspec <- reify spec
  prove prover scheme cspec
  --putStrLn $ K2.prettyPrint . K2.toKind2 K2.Modular . TS.translate $ cspec
  return ()

--------------------------------------------------------------------------------
