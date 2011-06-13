--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

-- |

module Copilot.Language.Spec
  ( Spec
  , Copilot (..)
  , Let (..)
  , let_
  , lets
  , Trigger (..)
  , TriggerArg (..)
  , triggers
  , runSpec
  , trigger
  , arg
  ) where

import Control.Monad.Writer
import Data.List (foldl')

import Copilot.Core (Typed)
import qualified Copilot.Core as Core
import Copilot.Language.Stream

--------------------------------------------------------------------------------

type Spec = Writer [Copilot] ()

--------------------------------------------------------------------------------

runSpec :: Spec -> [Copilot]
runSpec = execWriter 

--------------------------------------------------------------------------------

lets :: [Copilot] -> [Let]
lets = 
  foldl' lets' []
  where
  lets' ls e = case e of 
                 LetExpr l -> l : ls
                 _         -> ls

triggers :: [Copilot] -> [Trigger]
triggers = 
  foldl' triggers' []
  where
  triggers' ls e = case e of 
                     TriggerExpr t -> t : ls
                     _             -> ls

--------------------------------------------------------------------------------

data Copilot = 
    LetExpr Let
  | TriggerExpr Trigger

--------------------------------------------------------------------------------

data Let where
  Let
    :: Typed a
    => String
    -> Stream a
    -> Let

--------------------------------------------------------------------------------

let_ 
  :: Typed a
  => String
  -> Stream a
  -> Spec
let_ var e = tell [LetExpr $ Let var e]

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
trigger name e args = tell [TriggerExpr $ Trigger name e args]

--------------------------------------------------------------------------------

arg :: Typed a => Stream a -> TriggerArg
arg = TriggerArg

--------------------------------------------------------------------------------
