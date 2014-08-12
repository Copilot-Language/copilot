--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Main (main) where

import Language.Copilot as CP
import Copilot.Kind
import Copilot.Kind.Light
import Copilot.Kind.Kind2

import qualified Copilot.Kind.IL        as IL
import qualified Copilot.Kind.Light     as Light

import qualified Copilot.Kind.TransSys  as TS

import BoyerMoore

--------------------------------------------------------------------------------

line = replicate 40 '-'

prover = lightProver def {debugMode = True, kTimeout = 100}
--prover = kind2Prover def
--prover = naiveProver def `combine` kind2Prover def {bmcMax = 20}
--prover = kind2Prover (def {bmcMax = 20})


main :: IO ()
main =  do
  cspec <- reify spec
  CP.prettyPrint spec
  interpret 10 spec
  putStrLn $ IL.prettyPrint $ IL.translate cspec
  putStrLn line
  prove prover scheme cspec
  --putStrLn $ K2.prettyPrint . K2.toKind2 K2.Inlined [] [] . TS.translate $ cspec
  --putStrLn $ TS.prettyPrint . TS.translate $ cspec
  return ()

--------------------------------------------------------------------------------
