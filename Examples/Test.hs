--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | A test suite to check for basic functionality.

module Main where

import System.Directory ( removeDirectoryRecursive
                        , doesDirectoryExist
                        , doesFileExist
                        , removeFile )
import qualified Copilot.Compile.C99 as C99
import qualified Copilot.Compile.SBV as SBV
import Control.Monad (when)

import AddMult
import Array
--import BadExtVars
import Cast
import ClockExamples
import EngineExample
import Examples
import Examples2
import ExtFuns
import Local
import LTLExamples
import PTLTLExamples
import Random
import RegExpExamples
import StackExamples
import StatExamples
import VotingExamples


--------------------------------------------------------------------------------

main :: IO ()
main = do
  cleanup
  putStrLn "Testing addMult ..."
  addMult         >> cleanup
  putStrLn ""
  putStrLn "Testing array ..."
  array           >> cleanup
  putStrLn ""
--  putStrLn "Testing badExtVars ..."
--  badExtVars      >> cleanup
  putStrLn ""
  putStrLn "Testing castEx ..."
  castEx          >> cleanup
  putStrLn ""
  putStrLn "Testing clockExamples ..."
  clockExamples   >> cleanup
  putStrLn ""
  putStrLn "Testing engineExample ..."
  engineExample   >> cleanup
  putStrLn ""
  putStrLn "Testing examples ..."
  examples        >> cleanup
  putStrLn ""
  putStrLn "Testing examples2 ..."
  examples2       >> cleanup
  putStrLn ""
  putStrLn "Testing extFuns ..."
  extFuns         >> cleanup
  putStrLn ""
  putStrLn "Testing localEx ..."
  localEx         >> cleanup
  putStrLn ""
  putStrLn "Testing ltlExamples ..."
  ltlExamples     >> cleanup
  putStrLn ""
  putStrLn "Testing ptltlExamples ..."
  ptltlExamples   >> cleanup
  putStrLn ""
  putStrLn "Testing randomEx ..."
  randomEx        >> cleanup
  putStrLn ""
  putStrLn "Testing regExpExamples ..."
  regExpExamples  >> cleanup
  putStrLn ""
  putStrLn "Testing stackExamples ..."
  stackExamples   >> cleanup
  putStrLn ""
  putStrLn "Testing statExamples ..."
  statExamples    >> cleanup
  putStrLn ""
  putStrLn "Testing votingExamples ..."
  votingExamples  >> cleanup
  putStrLn ""
  putStrLn ""
  putStrLn "************************************"
  putStrLn " Ok, the basic tests passed.  Enjoy!"
  putStrLn "************************************"

--------------------------------------------------------------------------------

cleanup :: IO ()
cleanup = do
  b0 <- doesDirectoryExist SBV.sbvDirName
  when b0 (removeDirectoryRecursive SBV.sbvDirName)
  b1 <- doesDirectoryExist C99.c99DirName
  when b1 (removeDirectoryRecursive C99.c99DirName)
  let cbmc = "cbmc_driver.c"
  b2 <- doesFileExist cbmc
  when b2 (removeFile cbmc)

--------------------------------------------------------------------------------
