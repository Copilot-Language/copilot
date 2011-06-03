--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Intermediate representation for Copilot specifications.
-- The form of the representation is based on this paper:
--
-- * Carette, Jacques and Kiselyov, Oleg and Shan, Chung-chieh,
-- \"/Finally tagless, partially evaluated: Tagless staged/
-- /interpreters for simpler typed languages/\",
-- Journal of Functional Programming vol. 19, p. 509-543, 2009.
--
-- The following article might also be useful:
--
-- * Guillemette, Louis-Julien and Monnier, Stefan,
-- \"/Type-Safe Code Transformations in Haskell/\",
-- Electronic Notes in Theoretical Computer Science vol. 174, p. 23-39, 2007.
--
-- For examples of how to traverse a Copilot specification see
-- the source code of the interpreter
-- ("Copilot.Core.Interpret")
-- and the pretty-printer
-- ("Copilot.Core.PrettyPrint").

module Copilot.Core
  ( module Copilot.Core.Expr
  , module Copilot.Core.Spec
  , module Copilot.Core.Type
  , module Copilot.Core.Uninitialized
  , module Data.Int
  , module Data.Word
  ) where

import Copilot.Core.Expr
import Copilot.Core.Spec
import Copilot.Core.Type
import Copilot.Core.Uninitialized
import Data.Int
import Data.Word

--------------------------------------------------------------------------------