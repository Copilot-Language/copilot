--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Compile.C99
  ( compile
  ) where

import Copilot.Compile.C99.MetaTable (allocMetaTable)
import Copilot.Compile.C99.Phases (schedulePhases)
import Copilot.Compile.C99.PreCode (preCode)
import qualified Copilot.Core as Core
import Language.Atom (Atom)
import qualified Language.Atom as Atom

--------------------------------------------------------------------------------

compile :: String -> Core.Spec -> IO ()
compile programName spec =
  do
    (schedule, _, _, _, _) <- Atom.compile programName atomDefaults atomProgram
    putStrLn $ Atom.reportSchedule schedule

  where

  atomDefaults :: Atom.Config
  atomDefaults =
    Atom.defaults
      { Atom.cCode = \ _ _ _ -> (preCode spec, "") }

  atomProgram :: Atom ()
  atomProgram =
    do
      meta <- allocMetaTable spec
      schedulePhases meta spec

--------------------------------------------------------------------------------