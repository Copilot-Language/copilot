--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

-- |

module Copilot.Language.Compile
  ( compile
  ) where

import Copilot.Language.Spec (Spec)
import Copilot.Language.Reify (reify)
import qualified Copilot.Compile.C99 as C (compile)

--------------------------------------------------------------------------------

compile
  :: String
  -> Spec
  -> IO ()
compile name spec =
  reify spec >>= C.compile name

--------------------------------------------------------------------------------
