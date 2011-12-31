--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Compile.C99.Test.CheckSpec (checkSpec) where

import Copilot.Core ( Spec (..), Trigger(..))
import Copilot.Core.Expr (Name, UExpr (..))
import Copilot.Core.Type.Eq (UVal (..))
import Copilot.Core.Interpret.Eval (eval)
import Copilot.Compile.C99 (compile, c99DirName, c99FileRoot)
import Copilot.Compile.C99.Params (Params (..), defaultParams)
import Copilot.Compile.C99.Test.Driver (driver)
import Copilot.Compile.C99.Test.Iteration (Iteration(..), execTraceToIterations)
import Copilot.Compile.C99.Test.ReadCSV (iterationsFromCSV)
import Copilot.Core.Type.Show (ShowType(..))
import Copilot.Core.Type.Read (readWithType)

import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.List (foldl')
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.IO as TIO
import System.Directory (removeDirectoryRecursive)
import System.Process (system, readProcess)
import Control.Monad (when, unless)
import Text.PrettyPrint (text, (<+>), ($$), render, vcat, hang)

--------------------------------------------------------------------------------

checkSpec :: Int -> Spec -> IO Bool
checkSpec numIterations spec = do
  genCFiles numIterations spec
  compileCFiles
  csv <- execute numIterations
  let is1 = iterationsFromCSV csv
  let is2 = interp numIterations spec
  let eq = typedOutputs spec is1 == typedOutputs spec is2
  unless eq (putStrLn $ showCompare is1 is2)
  when eq cleanUp -- Keep things around if there's a failure
  return eq

--------------------------------------------------------------------------------

showCompare :: [Iteration] -> [Iteration] -> String
showCompare s1 s2 =
  render $ text "From C:" <+> text "---" <+> text "From Interpreter:" $$ 
    (vcat $ map (\(a,b) -> hang a 10 b) zipped)
  where
  zipped  = zip (toDoc s1) (toDoc s2)
  toDoc   = map (text . show) 

--------------------------------------------------------------------------------

-- mapping triggers to arg values
type TypedIteration = M.Map Name [UVal]

-- list of output values for comparison
typedOutputs :: Spec -> [Iteration] -> [TypedIteration]
typedOutputs Spec { specTriggers = triggers } = 
  map recoverTypes

  where
  recoverTypes :: Iteration -> TypedIteration
  recoverTypes Iteration { iterationOutputs = iterMap } = 
    M.mapWithKey recoverType iterMap

  recoverType :: Name -> [String] -> [UVal]
  recoverType trigName outs = 
    let types = typedTriggerArgs M.! trigName in
    map typedRead (zip types outs)

  typedRead :: (UExpr, String) -> UVal
  typedRead (UExpr { uExprType = t }, output) = 
    UVal { uType = t
         , uVal = readWithType t output }

  typedTriggerArgs :: M.Map Name [UExpr]
  typedTriggerArgs = 
    foldl' mkMap M.empty triggers
    where 
    mkMap mp trig = M.insert (triggerName trig) (triggerArgs trig) mp

--------------------------------------------------------------------------------

driverFile :: String
driverFile = "driver.c"

outputFile :: String
outputFile = "_test"

--------------------------------------------------------------------------------

genCFiles :: Int -> Spec -> IO ()
genCFiles numIterations spec = do
  compile (defaultParams { prefix = Nothing, verbose = False }) spec
  TIO.writeFile (c99DirName ++ "/" ++ driverFile) 
                (driver numIterations spec)

--------------------------------------------------------------------------------

compileCFiles :: IO ()
compileCFiles = do
  _ <- system $ unwords [ "cd " ++ c99DirName ++ ";"
                        , "gcc"
                        , c99FileRoot ++ ".c" 
                        , driverFile 
                        , "-o"
                        , outputFile ]
  return ()

--------------------------------------------------------------------------------

execute :: Int -> IO ByteString
execute _ =
  fmap B.pack (readProcess ("./" ++ c99DirName ++ "/" ++ outputFile) [] "")

--------------------------------------------------------------------------------

interp :: Int -> Spec -> [Iteration]
interp numIterations = 
  execTraceToIterations . eval C numIterations 

--------------------------------------------------------------------------------

cleanUp :: IO ()
cleanUp = removeDirectoryRecursive c99DirName
  -- removeFile $ c99FileRoot ++ ".c"
  -- removeFile $ c99FileRoot ++ ".h"
  -- removeFile driverFile
  -- removeFile outputFile

--------------------------------------------------------------------------------
