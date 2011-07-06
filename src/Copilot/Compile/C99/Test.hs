--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.Test (testCompilerAgainstInterpreter) where

import Copilot.Core (Spec)
import Copilot.Compile.C99.Test.CheckSpec (checkSpec)

testCompilerAgainstInterpreter :: Int -> Spec -> IO Bool
testCompilerAgainstInterpreter = checkSpec
