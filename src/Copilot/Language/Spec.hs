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

newtype Spec a = Spec { unSpec :: Writer [Trigger] a }
  deriving Monad

--------------------------------------------------------------------------------

runSpec :: Spec () -> [Trigger]
runSpec = execWriter . unSpec

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
  -> Spec ()
trigger name e args = Spec $ tell [Trigger name e args]

--------------------------------------------------------------------------------

arg :: Typed a => Stream a -> TriggerArg
arg = TriggerArg

--------------------------------------------------------------------------------
