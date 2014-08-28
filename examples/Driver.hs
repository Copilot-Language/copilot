--------------------------------------------------------------------------------

module Main (main) where

import Language.Copilot as CP
import Copilot.Kind
import Copilot.Kind.Light
import Copilot.Kind.Kind2

import qualified Copilot.Kind.IL        as IL
import qualified Copilot.Kind.Light     as Light
import qualified Copilot.Kind.Kind2     as K2

import qualified Copilot.Kind.TransSys  as TS

--------------------------------------------------------------------------------

-- Load here the example file you want to run :

import Fib

--------------------------------------------------------------------------------

--prover = kind2Prover def
--prover = lightProver def {onlyBmc = True, kTimeout = 5}
--prover = lightProver def { debugMode = True }
prover =
  lightProver def {debugMode = True} 
  `combine` kind2Prover def

main :: IO ()
main =  do
  cspec <- reify spec
  putStrLn $ IL.prettyPrint $ IL.translate cspec
  putStrLn line
  putStrLn $ TS.prettyPrint . TS.translate $ cspec
  putStrLn line
  putStrLn $ TS.prettyPrint . TS.complete . TS.removeCycles . TS.translate $ cspec
  prove prover scheme cspec
  
  where line = replicate 79 '-'

--------------------------------------------------------------------------------
