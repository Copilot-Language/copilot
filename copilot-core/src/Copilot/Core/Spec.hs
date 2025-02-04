{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE Safe                      #-}

-- |
-- Copyright: (c) 2011 National Institute of Aerospace / Galois, Inc.
--
-- Copilot specifications constitute the main declaration of Copilot modules.
--
-- A specification normally contains the association between streams to monitor
-- and their handling functions, or streams to observe, or a theorem that must
-- be proved.
--
-- In order to be executed, high-level Copilot Language Spec must be turned
-- into Copilot Core's 'Spec'. This module defines the low-level Copilot Core
-- representations for Specs and the main types of element in a spec.
module Copilot.Core.Spec
    ( Stream (..)
    , Observer (..)
    , Trigger (..)
    , Spec (..)
    , Property (..)
    , Prop (..)
    , extractProp
    )
  where

-- External imports
import Data.Typeable (Typeable)

-- Internal imports
import Copilot.Core.Expr (Expr, Id, Name, UExpr)
import Copilot.Core.Type (Type, Typed)

-- | A stream in an infinite succession of values of the same type.
--
-- Stream can carry different types of data. Boolean streams play a special
-- role: they are used by other parts (e.g., 'Trigger') to detect when the
-- properties being monitored are violated.
data Stream = forall a . (Typeable a, Typed a) => Stream
  { streamId       :: Id
  , streamBuffer   :: [a]
  , streamExpr     :: Expr a
  , streamExprType :: Type a
  }

-- | An observer, representing a stream that we observe during interpretation
-- at every sample.
data Observer = forall a . Typeable a => Observer
  { observerName     :: Name
  , observerExpr     :: Expr a
  , observerExprType :: Type a
  }

-- | A trigger, representing a function we execute when a boolean stream becomes
-- true at a sample.
data Trigger = Trigger
  { triggerName  :: Name
  , triggerGuard :: Expr Bool
  , triggerArgs  :: [UExpr]
  }

-- | A property, representing a boolean stream that is existentially or
-- universally quantified over time.
data Property = Property
  { propertyName :: Name
  , propertyProp :: Prop
  }

-- | A proposition, representing a boolean stream that is existentially or
-- universally quantified over time.
data Prop
  = Forall (Expr Bool)
  | Exists (Expr Bool)

-- | Extract the underlying stream from a quantified proposition.
--
-- Think carefully before using this function, as this function will remove the
-- quantifier from a proposition. Universally quantified streams usually require
-- separate treatment from existentially quantified ones, so carelessly using
-- this function to remove quantifiers can result in hard-to-spot soundness
-- bugs.
extractProp :: Prop -> Expr Bool
extractProp (Forall e) = e
extractProp (Exists e) = e

-- | A Copilot specification is a list of streams, together with monitors on
-- these streams implemented as observers, triggers or properties.
data Spec = Spec
  { specStreams    :: [Stream]
  , specObservers  :: [Observer]
  , specTriggers   :: [Trigger]
  , specProperties :: [Property]
  }
