--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Copilot specifications.

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Copilot.Language.Spec
  ( Spec
  , runSpec
  , SpecItem
  , Observer (..)
  , observer
  , observers
  , Trigger (..)
  , triggers
  , trigger
  , arg
  , Property (..)
  , prop
  , properties
  ) where

import Control.Monad.Writer
import Data.List (foldl')
--import Data.Maybe (fromMaybe)

--import Copilot.Core (Typed, Struct)
import Copilot.Core (Typed)
import qualified Copilot.Core as Core
import Copilot.Language.Stream

--------------------------------------------------------------------------------

type Spec = Writer [SpecItem] ()

--------------------------------------------------------------------------------

runSpec :: Spec -> [SpecItem]
runSpec = execWriter 

--------------------------------------------------------------------------------

observers :: [SpecItem] -> [Observer]
observers = 
  foldl' lets' []
  where
  lets' ls e =
    case e of 
      ObserverItem l -> l : ls
      _              -> ls

triggers :: [SpecItem] -> [Trigger]
triggers = 
  foldl' triggers' []
  where
  triggers' ls e =
    case e of 
      TriggerItem t -> t : ls
      _             -> ls

properties :: [SpecItem] -> [Property]
properties =
  foldl' properties' []
  where
  properties' ls e =
    case e of
      PropertyItem p -> p : ls
      _              -> ls

--------------------------------------------------------------------------------

data SpecItem
  = ObserverItem Observer
  | TriggerItem  Trigger
  | PropertyItem Property

--------------------------------------------------------------------------------

data Observer where
  Observer :: Typed a => String -> Stream a -> Observer

--------------------------------------------------------------------------------

data Property where
  Property :: String -> Stream Bool -> Property

--------------------------------------------------------------------------------

prop :: String -> Stream Bool -> Spec
prop name e = tell [PropertyItem $ Property name e]

--------------------------------------------------------------------------------

observer :: Typed a => String -> Stream a -> Spec
observer name e = tell [ObserverItem $ Observer name e]

--------------------------------------------------------------------------------

data Trigger where
  Trigger :: Core.Name -> Stream Bool -> [Arg] -> Trigger

--------------------------------------------------------------------------------

trigger :: String -> Stream Bool -> [Arg] -> Spec
trigger name e args = tell [TriggerItem $ Trigger name e args]

--------------------------------------------------------------------------------

arg :: Typed a => Stream a -> Arg
arg = Arg

--------------------------------------------------------------------------------

{-
-- | Struct operator.

-- Look up the given struct x, and return field y (which should be a stream?)
(#) :: Typed a => Core.StructData -> String -> Stream a
(Core.StructData {Core.structName = x, Core.structArgs = y})#z = getField x z
  where
    getField struct_nm field_nm =
      let test = find (\(Core.StructData name _) -> name == struct_nm) structs in
      case test of
        Nothing -> error "No struct named \"" ++ struct_nm ++ "\" in the spec"
        Just element ->
          fromMaybe (find (\(Core.SExpr name _) -> name == field_nm) (element Core.structArgs))
            (error "No field by the name of \"" ++ field_nm ++ "\"") element
--(Core.StructData l m)#n = Op2 (Core.GetField Core.typeOf) (Core.StructData l m) n
-}
--------------------------------------------------------------------------------