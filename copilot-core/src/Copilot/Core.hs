-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | Intermediate representation for Copilot specifications.
-- The following articles might also be useful:
--
-- * Carette, Jacques and Kiselyov, Oleg and Shan, Chung-chieh,
-- \"/Finally tagless, partially evaluated: Tagless staged/
-- /interpreters for simpler typed languages/\",
-- Journal of Functional Programming vol. 19, p. 509-543, 2009.
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

{-# LANGUAGE Safe #-}
-- The following warning is enabled in this module so that the import of
-- Copilot.Core.External does not give rise to a warning. It can be removed
-- when that module is removed from the implementation.
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Copilot.Core
  ( module Copilot.Core.Expr
  , module Copilot.Core.External
  , module Copilot.Core.Operators
  , module Copilot.Core.Spec
  , module Copilot.Core.Type
  , module Copilot.Core.Type.Array
  , module Data.Int
  , module Data.Word
  ) where

-- External imports
import Data.Int
import Data.Word

-- Internal imports
import Copilot.Core.Expr
import Copilot.Core.External -- See GHC flag enabled above
import Copilot.Core.Operators
import Copilot.Core.Spec
import Copilot.Core.Type
import Copilot.Core.Type.Array
