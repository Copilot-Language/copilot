--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Compile.C99
  ( compile
  ) where

import Copilot.Compile.Header.C99 (genC99Header)
import Copilot.Compile.C99.MetaTable (allocMetaTable)
import Copilot.Compile.C99.Phases (schedulePhases)
import Copilot.Compile.C99.PrePostCode (preCode, postCode)
import qualified Copilot.Core as Core
import Language.Atom (Atom)
import qualified Language.Atom as Atom

--------------------------------------------------------------------------------

compile :: String -> Core.Spec -> IO ()
compile programName spec =
  do
    (schedule, _, _, _, _) <- Atom.compile programName atomDefaults atomProgram
    putStrLn $ Atom.reportSchedule schedule
    genC99Header programName spec

  where

  atomDefaults :: Atom.Config
  atomDefaults =
    Atom.defaults
      { Atom.cCode = \ _ _ _ ->
        (preCode programName spec, postCode programName spec) }

  atomProgram :: Atom ()
  atomProgram =
    do
      meta <- allocMetaTable spec
      schedulePhases meta spec

--------------------------------------------------------------------------------