module Copilot.Compile.C99.Test.Property where

import System.Process                   (readProcess)

import Copilot.Core                     (Spec)
import Copilot.Compile.C99              (compile)

import Copilot.Compile.C99.Test.Driver


-- | Compile the specification and generate the test driver, then write these
-- to specname.[ch] and a main file.
writetest :: String -> String -> Spec -> Int -> IO ()
writetest specname mainfile spec iters = do
  let drivercode = writedriver specname spec iters
  writeFile mainfile drivercode
  compile specname spec


-- | Compile the C code to using GCC to a binary.
-- This function fails if the files names specname.c and mainfile do not exist.
compiletest :: String -> String -> IO String
compiletest specname mainfile = do
  let cflags = ["-Wall", "-pedantic-errors"]
      output = ["-o", specname]
      cfiles = [mainfile, specname ++ ".c"]
      args   = cflags ++ output ++ cfiles
  readProcess "gcc" args ""


-- | Run the compiled specification and driver.
runtest :: String -> IO String
runtest specname = readProcess ("./" ++ specname) [] ""
