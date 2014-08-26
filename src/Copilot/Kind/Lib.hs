--------------------------------------------------------------------------------

{-# LANGUAGE RebindableSyntax, ScopedTypeVariables #-}

module Copilot.Kind.Lib
  ( conj
  , disj
  , forAllCst
  , existsCst
  , arbitrary
  , arbitraryCst
  ) where

import Prelude ()
import Copilot.Language
import Copilot.Core.Type
import Copilot.Core.Type.Uninitialized

--------------------------------------------------------------------------------

conj :: [Stream Bool] -> Stream Bool
conj = foldl (&&) true

disj :: [Stream Bool] -> Stream Bool
disj = foldl (||) false

forAllCst ::(Typed a) => [a] -> (Stream a -> Stream Bool) -> Stream Bool
forAllCst l f = conj $ map (f . constant) l

existsCst :: (Typed a) => [a] -> (Stream a -> Stream Bool) -> Stream Bool
existsCst l f = disj $ map (f . constant) l

-- Arbitrary streams are built using the "extern" construct
-- A cleaner solution should be implemented

arbitrary :: (Typed a) => String -> Stream a
arbitrary s = extern s Nothing

-- A (very) dirty hack to build a constant stream of unknown value

arbitraryCst :: forall a . (Typed a) => String -> Stream a
arbitraryCst s = c
  where 
    t :: Stream Word8
    t = [0] ++ (1 + t)
    i :: Stream a
    i = extern s Nothing
    c = if t == 0 then i else [uninitialized (typeOf :: Type a)] ++ c
    
--------------------------------------------------------------------------------
        