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
data Stream = forall α . Show α => Stream
  { streamId       :: Id
  , streamBuffer   :: [α]
  , streamGuard    :: forall η . Expr η => Maybe (η Bool)
  , streamExpr     :: forall η . Expr η => η α
  , streamExprType :: Type α }

--------------------------------------------------------------------------------

-- | A trigger.
data Trigger = Trigger
  { triggerName    :: Name
  , triggerGuard   :: forall η . Expr η => η Bool
  , triggerArgs    :: [TriggerArg] }

--------------------------------------------------------------------------------

-- | An argument to a trigger.
data TriggerArg = forall α . Show α => TriggerArg
  { triggerArgExpr :: forall η . Expr η => η α
  , triggerArgType :: Type α }

--------------------------------------------------------------------------------

-- | A Copilot specification consists of a
-- list of anomymous streams and a list of triggers.
data Spec = Spec
  { specStreams    :: [Stream]
  , specTriggers   :: [Trigger] }

--------------------------------------------------------------------------------