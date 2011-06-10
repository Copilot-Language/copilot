--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Core.Spec
  ( Stream (..)
  , Observer (..)
  , Trigger (..)
  , TriggerArg (..)
  , Spec (..)
  ) where

import Copilot.Core.Expr (Name, Id, Expr)
import Copilot.Core.Type (Type)

--------------------------------------------------------------------------------

-- | A stream.
data Stream = forall a  . Stream
  { streamId         :: Id
  , streamBuffer     :: [a]
  , streamGuard      :: forall e . Expr e => Maybe (e Bool)
  , streamExpr       :: forall e . Expr e => e a
  , streamExprType   :: Type a }

--------------------------------------------------------------------------------

-- | An observer.
data Observer = forall a . Observer
  { observerName     :: Name
  , observerExpr     :: forall e . Expr e => e a
  , observerExprType :: Type a }

--------------------------------------------------------------------------------

-- | A trigger.
data Trigger = Trigger
  { triggerName      :: Name
  , triggerGuard     :: forall e . Expr e => e Bool
  , triggerArgs      :: [TriggerArg] }

--------------------------------------------------------------------------------

-- | An argument to a trigger.
data TriggerArg = forall a . TriggerArg
  { triggerArgExpr   :: forall e . Expr e => e a
  , triggerArgType   :: Type a }

--------------------------------------------------------------------------------

-- | A Copilot specification consists of a list of anomymous streams, a list of
-- observers, and a list of triggers.
data Spec = Spec
  { specStreams      :: [Stream]
  , specObservers    :: [Observer]
  , specTriggers     :: [Trigger] }

--------------------------------------------------------------------------------