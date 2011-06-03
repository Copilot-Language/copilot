--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Spec
  ( Stream (..)
  , Trigger (..)
  , TriggerArg (..)
  , Spec (..)
  ) where

import Copilot.Core.Expr (Name, Id, Expr)
import Copilot.Core.Type (Type)

--------------------------------------------------------------------------------

-- | A stream.
data Stream = forall a . Show a => Stream
  { streamId       :: Id
  , streamBuffer   :: [a]
  , streamGuard    :: forall e . Expr e => Maybe (e Bool)
  , streamExpr     :: forall e . Expr e => e a
  , streamExprType :: Type a }

--------------------------------------------------------------------------------

-- | A trigger.
data Trigger = Trigger
  { triggerName    :: Name
  , triggerGuard   :: forall e . Expr e => e Bool
  , triggerArgs    :: [TriggerArg] }

--------------------------------------------------------------------------------

-- | An argument to a trigger.
data TriggerArg = forall a . Show a => TriggerArg
  { triggerArgExpr :: forall e . Expr e => e a
  , triggerArgType :: Type a }

--------------------------------------------------------------------------------

-- | A Copilot specification consists of a
-- list of anomymous streams and a list of triggers.
data Spec = Spec
  { specStreams    :: [Stream]
  , specTriggers   :: [Trigger] }

--------------------------------------------------------------------------------