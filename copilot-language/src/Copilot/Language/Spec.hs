-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE Safe           #-}

-- | Copilot specifications consistute the main declaration of Copilot modules.
--
-- A specification normally contains the association between streams to monitor
-- and their handling functions, or streams to observe, or a theorem that must
-- be proved.
--
-- In order to be executed, 'Spec's must be turned into Copilot Core (see
-- 'Reify') and either simulated or converted into C99 code to be executed.
module Copilot.Language.Spec
  ( Spec, Spec'
  , runSpec
  , SpecItem
  , Observer (..)
  , observer, observers
  , Trigger (..)
  , trigger, triggers
  , arg
  , Property (..)
  , Prop (..)
  , prop, properties
  , theorem, theorems
  , forall, exists
  , extractProp
  , Universal, Existential
  ) where

import Prelude hiding (not)

import Control.Monad.Writer

import Copilot.Core (Typed)
import qualified Copilot.Core as Core
import Copilot.Language.Stream

import Copilot.Theorem.Prove

-- | A specification is a list of declarations of triggers, observers,
-- properties and theorems.
--
-- Specifications are normally declared in monadic style, for example:
--
-- @
-- monitor1 :: Stream Bool
-- monitor1 = [False] ++ not monitor1
--
-- counter :: Stream Int32
-- counter = [0] ++ not counter
--
-- spec :: Spec
-- spec = do
--   trigger "handler_1" monitor1 []
--   trigger "handler_2" (counter > 10) [arg counter]
-- @
type Spec = Writer [SpecItem] ()

-- | An action in a specification (e.g., a declaration) that returns a value that
-- can be used in subsequent actions.
type Spec' a = Writer [SpecItem] a

-- | Return the complete list of declarations inside a 'Spec' or 'Spec''.
--
-- The word run in this function is unrelated to running the underlying
-- specifications or monitors, and is merely related to the monad defined by a
-- 'Spec'
runSpec :: Spec' a -> [SpecItem]
runSpec = execWriter

-- | Filter a list of spec items to keep only the observers.
observers :: [SpecItem] -> [Observer]
observers =
  foldr lets' []
  where
  lets' e ls =
    case e of
      ObserverItem l -> l : ls
      _              -> ls

-- | Filter a list of spec items to keep only the triggers.
triggers :: [SpecItem] -> [Trigger]
triggers =
  foldr triggers' []
  where
  triggers' e ls =
    case e of
      TriggerItem t -> t : ls
      _             -> ls

-- | Filter a list of spec items to keep only the properties.
properties :: [SpecItem] -> [Property]
properties =
  foldr properties' []
  where
  properties' e ls =
    case e of
      PropertyItem p -> p : ls
      _              -> ls

-- | Filter a list of spec items to keep only the theorems.
theorems :: [SpecItem] -> [(Property, UProof)]
theorems =
  foldr theorems' []
  where
  theorems' e ls =
    case e of
      TheoremItem p -> p : ls
      _              -> ls

-- | An item of a Copilot specification.
data SpecItem
  = ObserverItem Observer
  | TriggerItem  Trigger
  | PropertyItem Property
  | TheoremItem (Property, UProof)

-- | An observer, representing a stream that we observe during execution at
-- every sample.
data Observer where
  Observer :: Typed a => String -> Stream a -> Observer

-- | Define a new observer as part of a specification. This allows someone to
-- print the value at every iteration during interpretation. Observers do not
-- have any functionality outside the interpreter.
observer :: Typed a
         => String    -- ^ Name used to identify the stream monitored in the
                      -- output produced during interpretation.
         -> Stream a  -- ^ The stream being monitored.
         -> Spec
observer name e = tell [ObserverItem $ Observer name e]

-- | A trigger, representing a function we execute when a boolean stream becomes
-- true at a sample.
data Trigger where
  Trigger :: Core.Name -> Stream Bool -> [Arg] -> Trigger

-- | Define a new trigger as part of a specification. A trigger declares which
-- external function, or handler, will be called when a guard defined by a
-- boolean stream becomes true.
trigger :: String       -- ^ Name of the handler to be called.
        -> Stream Bool  -- ^ The stream used as the guard for the trigger.
        -> [Arg]        -- ^ List of arguments to the handler.
        -> Spec
trigger name e args = tell [TriggerItem $ Trigger name e args]

-- | A property, representing a boolean stream that is existentially or
-- universally quantified over time.
data Property where
  Property :: String -> Stream Bool -> Property

-- | A proposition, representing the quantification of a boolean streams over
-- time.
data Prop a where
  Forall :: Stream Bool -> Prop Universal
  Exists :: Stream Bool -> Prop Existential

-- | Universal quantification of boolean streams over time.
forall :: Stream Bool -> Prop Universal
forall = Forall

-- | Existential quantification of boolean streams over time.
exists :: Stream Bool -> Prop Existential
exists = Exists

-- | Extract the underlying stream from a quantified proposition.
extractProp :: Prop a -> Stream Bool
extractProp (Forall p) = p
extractProp (Exists p) = p

-- | A proposition, representing a boolean stream that is existentially or
-- universally quantified over time, as part of a specification.
--
-- This function returns, in the monadic context, a reference to the
-- proposition.
prop :: String -> Prop a -> Writer [SpecItem] (PropRef a)
prop name e = tell [PropertyItem $ Property name (extractProp e)]
  >> return (PropRef name)

-- | A theorem, or proposition together with a proof.
--
-- This function returns, in the monadic context, a reference to the
-- proposition.
theorem :: String -> Prop a -> Proof a -> Writer [SpecItem] (PropRef a)
theorem name e (Proof p) = tell [TheoremItem (Property name (extractProp e), p)]
  >> return (PropRef name)

-- | Construct a function argument from a stream.
--
-- 'Arg's can be used to pass arguments to handlers or trigger functions, to
-- provide additional information to monitor handlers in order to address
-- property violations. At any given point (e.g., when the trigger must be
-- called due to a violation), the arguments passed using 'arg' will contain
-- the current samples of the given streams.
arg :: Typed a => Stream a -> Arg
arg = Arg
