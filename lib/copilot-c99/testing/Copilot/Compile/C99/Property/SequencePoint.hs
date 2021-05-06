module Copilot.Compile.C99.Property.SequencePoint where

import Copilot.Core.Interpret           (interpret, Format(..))
import Copilot.Language           as C
import Copilot.Language.Reify           (reify)

import Copilot.Compile.C99.Test


-- | The code generator does not produce code that raises -Wsequence-point
-- warnings in GCC.
-- We test this by generating code for a minimal specification that would raise
-- a warning. We compile the spec with the proper flags to the C compiler, in
-- case it compiles fine, we know that error didn't raise. In case of an
-- exception, we never reach `return True`.
-- Issue #34: Fix '-Wsequence-point' warnings from GCC
prop_sequencepoint :: IO Bool
prop_sequencepoint = do
  let specname = "sequencepoint_test"
      mainfile = specname Prelude.++ "_main.c"

  let testspec = trigger "sequencepoint_test" true [arg s0]
        where
          s0 :: Stream Int8
          s0 = [1, 2] C.++ s0 -- Buffer shoud hold >1 elems to raise warning.

  spec <- reify testspec
  writetest   specname mainfile spec 1
  compiletest specname mainfile ["-Wsequence-point", "-Werror"]
  return True
