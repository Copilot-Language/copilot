{-# LANGUAGE TypeSynonymInstances #-}

module Language.Copilot.Language.FunctionCalls 
  ((<>), trigger, void, fun) where

import Language.Copilot.Core

import Control.Monad.Writer (tell)
import qualified Data.Map as M

-- | No C arguments 
void :: Args
void = []

class ArgCl a where 
  (<>) :: Streamable b => Spec b -> a -> Args
  trigger :: Spec Bool -> String -> a -> Streams
  fun :: String -> a -> Ext

instance ArgCl Args where 
  s <> args = argUpdate s args
  trigger v fnName args = trigger' v fnName args
  fun = Fun

instance Streamable b => ArgCl (Spec b) where 
  s <> s' = (argUpdate s (argUpdate s' []))
  trigger v fnName arg = 
    let arg' = argUpdate arg [] in
    trigger' v fnName arg'
  fun f arg = 
    let arg' = argUpdate arg [] in
    Fun f arg'

infixr 1 <>

-- | XXX document
trigger' :: Spec Bool -> String -> Args -> Streams
trigger' v fnName args =
  tell $ LangElems 
           emptySM 
           emptySM 
           (M.insert (show trig) trig M.empty)
  where trig = Trigger v fnName args

argUpdate :: Streamable a => Spec a -> Args -> Args
argUpdate s args = notConstVarErr s (\v -> v:args)

