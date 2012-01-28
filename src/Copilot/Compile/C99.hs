--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Compile through the Atom backend.

module Copilot.Compile.C99
  ( compile
  , c99DirName
  , c99FileRoot
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

import Data.Char (toUpper)
import Control.Monad (when, unless)
import System.Directory ( doesDirectoryExist
                        , createDirectory
                        , removeDirectoryRecursive
                        , renameFile
                        , removeFile)

--------------------------------------------------------------------------------

c99DirName :: String
c99DirName = "copilot-c99-codegen"

c99FileRoot :: String
c99FileRoot = "copilot"

--------------------------------------------------------------------------------

compile :: Params -> Core.Spec -> IO ()
compile params spec0 = do
  b <- doesDirectoryExist dirName
  when b $ do putStrLn "Directory exists.  Delete? [y/N]:"
              input <- getLine
              if (map toUpper input == "Y") 
                then (removeDirectoryRecursive dirName) >> build
                else putStrLn "Ok, nothing done.  Terminating."
  unless b build 
  where
  build = do 
    createDirectory dirName
    (schedule, _, _, _, _) <- Atom.compile programName atomDefaults atomProgram
    when (verbose params) $ putStrLn (Atom.reportSchedule schedule)
    genC99Header (prefix params) dirName spec
    mv ".c" -- the C file Atom generates
    removeFile (programName ++ ".h")  -- We don't want Atom's .h file, but our own

  mv ext = renameFile p (dirName ++ "/" ++ p)
    where p = programName ++ ext

  dirName = withPrefix (prefix params) c99DirName

  spec :: Core.Spec
  spec = Core.makeTags spec0

  programName :: String
  programName = withPrefix (prefix params) c99FileRoot

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
