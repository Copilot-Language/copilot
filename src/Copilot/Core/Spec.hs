--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Copilot.Core.Spec
  ( Stream (..)
  , Observer (..)
  , Trigger (..)
  , Spec (..)
  ) where

import Copilot.Core.Expr (Name, Id, Expr, UExpr)
import Copilot.Core.Type (Type, Typed)

--------------------------------------------------------------------------------

-- | A stream.
data Stream = forall a. Typed a => Stream
  { streamId         :: Id
  , streamBuffer     :: [a]
  , streamExpr       :: Expr a
  , streamExprType   :: Type a }

--------------------------------------------------------------------------------

-- | An observer.
data Observer = forall a. Observer
  { observerName     :: Name
  , observerExpr     :: Expr a
  , observerExprType :: Type a }

--------------------------------------------------------------------------------

-- | A trigger.
data Trigger = Trigger
  { triggerName      :: Name
  , triggerGuard     :: Expr Bool
  , triggerArgs      :: [UExpr] }

--------------------------------------------------------------------------------

-- | A Copilot specification consists of a list of variables bound to anonymous
-- streams, a lost of anomymous streams, a list of observers, and a list of
-- triggers.
data Spec = Spec
  { specStreams      :: [Stream]
  , specObservers    :: [Observer]
  , specTriggers     :: [Trigger] }

--------------------------------------------------------------------------------
