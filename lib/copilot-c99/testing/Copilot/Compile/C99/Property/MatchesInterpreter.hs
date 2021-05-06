module Copilot.Compile.C99.Property.MatchesInterpreter where

import Data.Either                      (isRight)

import Text.CSV
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- We import Copilot.Language vs Language.Copilot* as that would create a
-- dependency cycle. It forces us to import from multiple files however.
import Copilot.Core.Interpret           (interpret, Format(..))
import Copilot.Language                 (Spec)
import Copilot.Language.Reify           (reify)
import Copilot.Language.Arbitrary

import Copilot.Compile.C99.Test


-- | For a given specification, the output of the driver should match the
-- output of Copilots interpreter.
prop_matches_interpreter :: Spec -> Property
prop_matches_interpreter langspec = monadicIO $ do
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
