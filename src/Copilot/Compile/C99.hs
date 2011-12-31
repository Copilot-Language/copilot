--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Compile.C99
  ( compile
  , module Copilot.Compile.C99.Params
  ) where

import qualified Copilot.Core as Core
import Copilot.Compile.Header.C99 (genC99Header)
import Copilot.Compile.C99.MetaTable (allocMetaTable)
import Copilot.Compile.C99.Params
import Copilot.Compile.C99.Phases (schedulePhases)
import Copilot.Compile.C99.PrePostCode (preCode, postCode)


import Language.Atom (Atom)
import qualified Language.Atom as Atom

import Control.Monad (when)
import System.Directory (createDirectory)

--------------------------------------------------------------------------------

compile :: Params -> Core.Spec -> IO ()
compile params spec0 = do
  createDirectory dirName
  (schedule, _, _, _, _) <- Atom.compile programName atomDefaults atomProgram
  when (verbose params) $ putStrLn (Atom.reportSchedule schedule)
  genC99Header (prefix params) dirName spec

  where

  dirName = withPrefix (prefix params) "copilot-c99"

  spec :: Core.Spec
  spec = Core.makeTags spec0

  programName :: String
  programName = dirName ++ "/" ++ withPrefix (prefix params) "copilot-c99"

  atomDefaults :: Atom.Config
  atomDefaults =
    Atom.defaults
      { Atom.cCode = \ _ _ _ ->
        (preCode params spec, postCode params spec) }

  atomProgram :: Atom ()
  atomProgram =
    do
      meta <- allocMetaTable spec
      schedulePhases params meta spec
