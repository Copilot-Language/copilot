{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Copilot.Language.FunctionCalls
  ((<>), trigger, void, fun) where

import Language.Copilot.Core

import Control.Monad.Writer (tell)
import qualified Data.Map as M

-- | No C arguments
void :: Args
void = []


class ArgFunCl a where
  trigger :: Spec Bool -> String -> a -> Streams
  fun :: String -> a -> Ext

instance ArgFunCl Args where
  trigger v fnName args = trigger' v fnName args
  fun = Fun

instance Streamable a => ArgFunCl ( Spec a ) where
  trigger v fnName arg =
    let arg' = argUpdate arg [] in
    trigger' v fnName arg'
  fun f arg =
    let arg' = argUpdate arg [] in
    Fun f arg'

instance ArgFunCl String where
  trigger v fnName arg = trigger' v fnName [ S arg ]
  fun f arg = Fun f [ S arg ]


class ArgListCl a b where
  (<>) :: a -> b -> Args

instance Streamable a => ArgListCl ( Spec a ) Args where
  s <> args = argUpdate s args

instance ArgListCl String Args where
  s <> args = ( S s ) : args

instance ( Streamable a, Streamable b ) => ArgListCl ( Spec a ) ( Spec b ) where
  s <> s' = (argUpdate s (argUpdate s' []))

instance ( Streamable a ) => ArgListCl ( Spec a ) String where
  s <> s' =  argUpdate s [ S s' ]

instance ( Streamable a ) => ArgListCl String ( Spec a ) where
  s <> s' = ( S s ) : argUpdate s' []

instance ArgListCl String String where
  s <> s' = [ S s, S s' ]


infixr 1 <>

-- | XXX document
trigger' :: Spec Bool -> String -> Args -> Streams
trigger' v fnName args =
  tell $ LangElems
           emptySM
           (M.insert (show trig) trig M.empty)
  where trig = Trigger v fnName args

argUpdate :: Streamable a => Spec a -> Args -> Args
argUpdate s args = notConstVarErr s (\v -> v:args)
