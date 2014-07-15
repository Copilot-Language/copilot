--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Main (main) where

import qualified Copilot.Core as Core
import Copilot.Language.Reify
import Copilot.Core.Interpret
import Copilot.Core.PrettyPrint

--import qualified Copilot.Kind.TransSys as TS
--import qualified Copilot.Kind.Kind2
--import Copilot.Kind.Kind2.Prover
import Copilot.Kind.Naive
import Copilot.Kind.Prover

--import Copilot.Kind.Kind2 as K2

import qualified Copilot.Kind.Naive as Naive
import qualified Copilot.Kind.IL as IL
import qualified Copilot.Core.PrettyPrint as CPP

import Copilot.Kind.Prove

import Grey

--------------------------------------------------------------------------------

line = replicate 40 '-'

prover = naiveProver def
--prover = kind2Prover def
--prover = naiveProver def `combine` kind2Prover def {bmcMax = 20}
--prover = kind2Prover (def {bmcMax = 20})


main :: IO ()
main =  do
  cspec <- reify spec
  -- putStrLn $ IL.prettyPrint $ IL.translate cspec
  
  prove prover scheme cspec
  --putStrLn $ K2.prettyPrint . K2.toKind2 K2.Inlined [] [] . TS.translate $ cspec
  --putStrLn $ TS.prettyPrint . TS.translate $ cspec
  return ()

--------------------------------------------------------------------------------
