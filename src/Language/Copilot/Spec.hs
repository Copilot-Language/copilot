-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Language.Copilot.Spec
  ( Spec (..)
  , Map2
  , Key
  , lookup
  , fmap2
  ) where

import Language.Copilot.Node (Node)
import Language.Copilot.Reify (Map2, Key, lookup, fmap2)
import Language.Copilot.Streamable (Streamable)
import Prelude ()

--data Trigger where
--  Trigger :: Streamable a => String -> Key x a -> Trigger

data Spec a where
  Spec
    :: Streamable a
    => Map2 x (Node (Key x))
    -> Key x a
    -> Spec a
