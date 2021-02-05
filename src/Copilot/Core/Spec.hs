--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification, GADTs #-}

-- | Copilot specifications consistute the main declaration of Copilot modules.
--
-- A specification normally contains the association between streams to monitor
-- and their handling functions, or streams to observe, or a theorem that must
-- be proved.
--
-- In order to be executed, high-level Copilot Language Spec must be turned
-- into Copilot Core's 'Spec'. This module defines the low-level Copilot Core
-- representations for Specs and the main types of element in a spec..
module Copilot.Core.Spec
  ( Stream (..)
  , Observer (..)
  , Trigger (..)
  , Spec (..)
  , Property (..)
  ) where

import Copilot.Core.Expr (Name, Id, Expr, UExpr)
import Copilot.Core.Type (Type, Typed)
import Data.Typeable (Typeable)

--------------------------------------------------------------------------------

-- | A stream in an infinite succession of values of the same type.
--
-- Stream can carry different types of data. Boolean streams play a special
-- role: they are used by other parts (e.g., 'Trigger') to detect when the
-- properties being monitored are violated.
data Stream = forall a. (Typeable a, Typed a) => Stream
  { streamId         :: Id
  , streamBuffer     :: [a]
  , streamExpr       :: Expr a
  , streamExprType   :: Type a }

--------------------------------------------------------------------------------

-- | An observer, representing a stream that we observe during execution at
-- every sample.
data Observer = forall a. Typeable a => Observer
  { observerName     :: Name
  , observerExpr     :: Expr a
  , observerExprType :: Type a }

--------------------------------------------------------------------------------

-- | A trigger, representing a function we execute when a boolean stream becomes
-- true at a sample.
data Trigger = Trigger
  { triggerName      :: Name
  , triggerGuard     :: Expr Bool
  , triggerArgs      :: [UExpr] }

--------------------------------------------------------------------------------

-- | A property, representing a boolean stream that is existentially or
-- universally quantified over time.
data Property = Property
  { propertyName     :: Name
  , propertyExpr     :: Expr Bool }

--------------------------------------------------------------------------------

-- | A Copilot specification consists of a list of variables bound to anonymous
-- streams, a list of anomymous streams, a list of observers, a list of
-- triggers, and a list of structs.
data Spec = Spec
  { specStreams      :: [Stream]
  , specObservers    :: [Observer]
  , specTriggers     :: [Trigger]
  , specProperties   :: [Property] }

--------------------------------------------------------------------------------
