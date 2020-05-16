module Copilot.Compile.C99.Property (prop_matching_output) where

import Data.Either                      (isRight)
import System.Process                   (readProcess)

import Text.CSV
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- We import Copilot.Language vs Language.Copilot* as that would create a
-- dependency cycle. It forces us to import from multiple files however.
import Copilot.Core                     (Spec)
import Copilot.Core.Interpret           (interpret, Format(..))
import qualified Copilot.Language as L  (Spec)
import Copilot.Language.Reify           (reify)
import Copilot.Language.Arbitrary
import Copilot.Compile.C99              (compile)

import Copilot.Compile.C99.Driver


-- | Compile the specification and generate the test driver, then write these
-- to specname.[ch] and a main file.
writetest :: String -> String -> Spec -> Int -> IO ()
writetest specname mainfile spec iters = do
  let drivercode = writedriver specname spec iters
  writeFile mainfile drivercode
  compile specname spec


-- | Compile the C code to using GCC to a binary.
-- This function fails if the files names specname.c and mainfile do not exist.
compiletest :: String -> String -> [String] -> IO String
compiletest specname mainfile cflags = do
  let output = ["-o", specname]
      cfiles = [mainfile, specname ++ ".c"]
      args   = cflags ++ output ++ cfiles
  readProcess "gcc" args ""


-- | Run the compiled specification and driver.
runtest :: String -> IO String
runtest specname = readProcess ("./" ++ specname) [] ""


-- | For a given specification, the output of the driver should match the
-- output of Copilots interpreter.
prop_matching_output :: L.Spec -> Property
prop_matching_output langspec = monadicIO $ do
  let specname = "testspec"
      mainfile = specname ++ "_main.c"
      iters    = 30
  spec <- run $ reify langspec
  run $ writetest   specname mainfile spec iters
  run $ compiletest specname mainfile ["-Wall", "-pedantic-errors"]

  -- Run the interpreter and parse CSV.
  let interpout    = interpret CSV iters spec
      interpcsv    = parseCSV "" interpout
      interpparsed = counterexample "Interpreter output parse failed."
                      (isRight interpcsv)

  -- Run the test and parse CSV.
  c99out <- run $ runtest specname
  let c99csv    = init <$> parseCSV "" c99out
      c99parsed = counterexample "C99 output parse failed." (isRight c99csv)

  return $ c99parsed .&&. interpparsed .&&. c99csv === interpcsv
