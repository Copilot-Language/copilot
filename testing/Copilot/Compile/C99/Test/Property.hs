module Copilot.Compile.C99.Test.Property where

import Copilot.Core                     (Spec)
import Copilot.Compile.C99              (compile)

import Copilot.Compile.C99.Test.Driver


-- | Compile the specification and generate the test driver, then write these
-- to specname.[ch] and a main file.
writetest :: String -> String -> Spec -> IO ()
writetest specname mainfile spec = do
  let drivercode = writedriver specname spec 30
  writeFile mainfile drivercode
  compile specname spec
