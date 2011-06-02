--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Compile.C99
  ( compile
  ) where

import Copilot.Compile.C99.MetaTable (allocMetaTable)
import Copilot.Compile.C99.Phases (schedulePhases)
import qualified Copilot.Core as Core
import Language.Atom (Atom)
import qualified Language.Atom as Atom

--------------------------------------------------------------------------------

compile :: String -> Core.Spec -> IO ()
compile pname spec =
  do
    (schedule, _, _, _, _) <- Atom.compile pname Atom.defaults p
    putStrLn $ Atom.reportSchedule schedule

  where

  p :: Atom ()
  p =
    do
      meta <- allocMetaTable spec
      schedulePhases meta spec

--------------------------------------------------------------------------------