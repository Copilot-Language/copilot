-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | Let expressions.
--
-- Although Copilot is a DSL embedded in Haskell and Haskell does support let
-- expressions, we want Copilot to be able to implement sharing, to detect when
-- the same stream is being used in multiple places in a specification and
-- avoid recomputing it unnecessarily.

{-# LANGUAGE Safe #-}

module Copilot.Language.Operators.Local
  ( local
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream (Stream (..))

-- | Let expressions.
--
--   Create a stream that results from applying a stream to a function on
--   streams. Standard usage would be similar to Haskell's let. See the
--   following example, where @stream1@, @stream2@ and @s@ are all streams
--   carrying values of some numeric type:
--
--   @
--   expression = local (stream1 + stream2) $ \\s ->
--                (s >= 0 && s <= 10)
--   @
local :: (Typed a, Typed b) => Stream a -> (Stream a -> Stream b) -> Stream b
local = Local
