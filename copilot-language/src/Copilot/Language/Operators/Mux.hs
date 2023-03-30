-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE Safe #-}

-- | Pick values from one of two streams, depending whether a condition is true
-- or false.
module Copilot.Language.Operators.Mux
  ( mux
  , ifThenElse
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Prelude
import Copilot.Language.Stream
import Prelude ()
import GHC.Stack (HasCallStack)

-- | Convenient synonym for 'ifThenElse'.
mux :: (HasCallStack, Typed a) => Stream Bool -> Stream a -> Stream a -> Stream a
mux (Const True) t _  = t
mux (Const False) _ f = f
mux b t f             = Op3 (Core.Mux typeOf) b t f

-- | If-then-else applied point-wise to three streams (a condition stream, a
-- then-branch stream, and an else-branch stream).
--
-- Produce a stream that, at any point in time, if the value of the first
-- stream at that point is true, contains the value in the second stream at
-- that time, otherwise it contains the value in the third stream.
ifThenElse :: (HasCallStack, Typed a) => Stream Bool -> Stream a -> Stream a -> Stream a
ifThenElse = mux
