--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.Test.CheckSpec (checkSpec) where

import Copilot.Compile.C99 (compile)
import Copilot.Core (Spec)
import Copilot.Core.Interpret.Eval (eval)
import Copilot.Compile.C99.Test.Driver (driver)
import Copilot.Compile.C99.Test.Iteration (Iteration, execTraceToIterations)
import Copilot.Compile.C99.Test.ReadCSV (iterationsFromCSV)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.IO as TIO
import System.Directory (removeFile)
import System.Process (system, readProcess)

--------------------------------------------------------------------------------

checkSpec :: Int -> Spec -> IO Bool
checkSpec k spec =
  do
    genCFiles spec
    compileCFiles
    csv <- execute k
    let
      is1 = iterationsFromCSV csv
      is2 = interp k spec
    cleanUp
    return (is1 == is2)

genCFiles :: Spec -> IO ()
genCFiles spec =
  do
    compile "tmp_" spec
    TIO.writeFile "tmp_driver_.c" (driver "tmp_" spec)
    return ()

compileCFiles :: IO ()
compileCFiles =
  do
    _ <- system $ "gcc tmp_.c tmp_driver_.c -o tmp_"
    return ()

execute :: Int -> IO ByteString
execute k =
  do
    fmap B.pack (readProcess "./tmp_" [show k] "")

interp :: Int -> Spec -> [Iteration]
interp k = execTraceToIterations . eval k []

cleanUp :: IO ()
cleanUp =
  do
    removeFile "tmp_.c"
    removeFile "tmp_.h"
    removeFile "tmp_driver_.c"
    removeFile "tmp_"
    return ()
