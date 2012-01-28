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
import qualified Copilot.Tools.CBMC as M
import Control.Monad (when, unless)
import Data.Maybe (catMaybes)

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
  ls <- checkExists
  when (null ls) runTests
  unless (null ls) $ do putStrLn "*** Warning! ***"
                        putStrLn "Cannot run tests in this directory.  You have the following files or directories that would be deleted: "
                        mapM_ putStrLn ls
                                
runTests :: IO ()
runTests = do
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

cbmcName :: String
cbmcName = "cbmc_driver.c"

atomCBMC :: String
atomCBMC = M.appendPrefix M.atomPrefix C99.c99DirName

sbvCBMC :: String
sbvCBMC = M.appendPrefix M.sbvPrefix SBV.sbvDirName

--------------------------------------------------------------------------------

checkExists :: IO [String]
checkExists = do
  b0 <- nmBool doesDirectoryExist SBV.sbvDirName
  b1 <- nmBool doesDirectoryExist C99.c99DirName
  b2 <- nmBool doesFileExist cbmcName
  b3 <- nmBool doesDirectoryExist atomCBMC
  b4 <- nmBool doesDirectoryExist sbvCBMC
  return $ catMaybes $ map getName [b0, b1, b2, b3, b4]

  where
  getName (nm, bool) = if bool then Just nm else Nothing
  nmBool f nm = do b <- f nm 
                   return (nm, b)

--------------------------------------------------------------------------------

cleanup :: IO ()
cleanup = do

  b0 <- doesDirectoryExist SBV.sbvDirName
  when b0 (removeDirectoryRecursive SBV.sbvDirName)

  b1 <- doesDirectoryExist C99.c99DirName
  when b1 (removeDirectoryRecursive C99.c99DirName)

  b2 <- doesFileExist cbmcName
  when b2 (removeFile cbmcName)

  b3 <- doesDirectoryExist atomCBMC
  when b3 (removeDirectoryRecursive atomCBMC)

  b4 <- doesDirectoryExist sbvCBMC
  when b4 (removeDirectoryRecursive sbvCBMC)

--------------------------------------------------------------------------------
