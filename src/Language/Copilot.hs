-- | Copilot is a stream-based runtime verification framework. Programs can be
-- interpreted for testing, or translated into C99 code to be incorporated in a
-- project, or as a standalone application. The C99 backend output is constant
-- in memory and time, making it suitable for systems with hard realtime
-- requirements.
--
-- This module is the main entry point for the Copilot language. The
-- expectation is that most Copilot users will only need to import this module,
-- together with one of the backend modules (at present, only
-- 'Copilot.Compile.C99' from the
-- <https://hackage.haskell.org/package/copilot-c99 copilot-c99> library is
-- available).
module Language.Copilot
  (
    module Copilot.Language
  , module Copilot.Language.Prelude
  , module Copilot.Language.Reify

  -- Code generators
  -- , module Copilot.Compile.C99
  -- , module Copilot.Compile.SBV

  -- Libraries
  , module Copilot.Library.Libraries
  -- , module Copilot.Library.LTL
  -- , module Copilot.Library.PTLTL
  -- , module Copilot.Library.Statistics
  -- , module Copilot.Library.RegExp
  -- , module Copilot.Library.Utils
  -- , module Copilot.Library.Voting
  -- , module Copilot.Library.Stacks

  , copilotMain
  , defaultMain
  ) where

import Copilot.Language
import Copilot.Language.Prelude
import Copilot.Language.Reify

-- Code generators
-- import Copilot.Compile.C99
-- import Copilot.Compile.SBV

-- Libraries
import Copilot.Library.Libraries
-- import Copilot.Library.Clocks
-- import Copilot.Library.LTL
-- import Copilot.Library.PTLTL
-- import Copilot.Library.Statistics
-- import Copilot.Library.RegExp
-- import Copilot.Library.Utils
-- import Copilot.Library.Voting
-- import Copilot.Library.Stacks

import Language.Copilot.Main
