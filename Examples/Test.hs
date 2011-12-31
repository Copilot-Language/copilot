--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | A test suite to check for basic functionality.

module Main where

import System.Directory (removeFile, removeDirectoryRecursive)

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
  putStrLn "Testing addMult ..."
  addMult         >> sbvCleanup
  putStrLn "Testing array ..."
  array           >> cleanup
  putStrLn "Testing badExtVars ..."
--  badExtVars      >> cleanup
  putStrLn "Testing castEx ..."
  castEx          >> cleanup
  putStrLn "Testing clockExamples ..."
  clockExamples   >> cleanup
  putStrLn "Testing engineExample ..."
  engineExample   >> cleanup
  putStrLn "Testing examples ..."
  examples        >> sbvCleanup
  putStrLn "Testing examples2 ..."
  examples2       >> sbvCleanup
  putStrLn "Testing extFuns ..."
  extFuns         >> cleanup
  putStrLn "Testing localEx ..."
  localEx         >> cleanup
  putStrLn "Testing ltlExamples ..."
  ltlExamples     >> cleanup
  putStrLn "Testing ptltlExamples ..."
  ptltlExamples   >> cleanup
  putStrLn "Testing randomEx ..."
  randomEx        >> cleanup
  putStrLn "Testing regExpExamples ..."
  regExpExamples  >> cleanup
  putStrLn "Testing stackExamples ..."
  stackExamples   >> cleanup
  putStrLn "Testing statExamples ..."
  statExamples    >> cleanup
  putStrLn "Testing votingExamples ..."
  votingExamples  >> cleanup

  putStrLn "*********************************"
  putStrLn " Ok, things seem to work.  Enjoy!"
  putStrLn "*********************************"

--------------------------------------------------------------------------------

sbvCleanup :: IO ()
sbvCleanup = removeDirectoryRecursive "copilot"

cleanup :: IO ()
cleanup = do
  --
  -- removeFile "copilot.c"
  -- removeFile "copilot.h"
  return ()

--------------------------------------------------------------------------------
