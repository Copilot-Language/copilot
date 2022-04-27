-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | Label a stream with additional information.

{-# LANGUAGE Safe #-}

module Copilot.Language.Operators.Label
  ( label
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream (Stream (..))

-- | This function allows you to label a stream with a tag, which can be used
-- by different backends to provide additional information either in error
-- messages or in the generated code (e.g., for traceability purposes).
--
-- Semantically, a labelled stream is just the stream inside it. The use of
-- label should not affect the observable behavior of the monitor, and how it
-- is used in the code generated is a decision specific to each backend.
label :: (Typed a) => String -> Stream a -> Stream a
label = Label
