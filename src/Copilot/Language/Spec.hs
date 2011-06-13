--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

-- |

module Copilot.Language.Spec
  ( Spec
  , Trigger (..)
  , TriggerArg (..)
  , runSpec
  , trigger
  , arg
  ) where

import Control.Monad.Writer
import Copilot.Core (Typed)
import qualified Copilot.Core as Core
import Copilot.Language.Stream

--------------------------------------------------------------------------------

type Spec = Writer [Trigger] ()

--------------------------------------------------------------------------------

runSpec :: Spec -> [Trigger]
runSpec = execWriter 

--------------------------------------------------------------------------------

data Trigger where
  Trigger
    :: Core.Name
    -> Stream Bool
    -> [TriggerArg]
    -> Trigger

--------------------------------------------------------------------------------

data TriggerArg where
  TriggerArg
    :: Typed a
    => Stream a
    -> TriggerArg

--------------------------------------------------------------------------------

trigger
  :: String
  -> Stream Bool
  -> [TriggerArg]
  -> Spec 
trigger name e args = tell [Trigger name e args]

--------------------------------------------------------------------------------

arg :: Typed a => Stream a -> TriggerArg
arg = TriggerArg

--------------------------------------------------------------------------------
