--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

-- | 

module Copilot.Core.Spec.Externals
  ( Extern (..)
  , externals
  ) where

import Copilot.Core
import Data.List (foldr)
import Prelude hiding ((++), concat, foldr)

--------------------------------------------------------------------------------

data Extern = forall a . Extern
  { externName :: Name
  , externType :: Type a }

--------------------------------------------------------------------------------

externals :: Spec -> [Extern]
externals Spec { specStreams  = streams, specTriggers = triggers } =
  toList $
    concat (fmap extsStream  streams) ++ concat (fmap extsTrigger triggers)

--------------------------------------------------------------------------------

extsStream :: Stream -> DList Extern
extsStream Stream { streamExpr = e } = extsExpr e

--------------------------------------------------------------------------------

extsTrigger :: Trigger -> DList Extern
extsTrigger Trigger { triggerGuard = e, triggerArgs = args } =
  extsExpr e ++ concat (fmap extsTriggerArg args)

  where

  extsTriggerArg :: TriggerArg -> DList Extern
  extsTriggerArg (TriggerArg e1 _) = extsExpr e1

--------------------------------------------------------------------------------

newtype ExtsExpr α = ExtsExpr { extsExpr :: DList Extern }

instance Expr ExtsExpr where
  const  _ _     = ExtsExpr $ empty
  drop   _ _ _   = ExtsExpr $ empty
  extern t name  = ExtsExpr $ singleton (Extern name t)
  op1 _ e        = ExtsExpr $ extsExpr e
  op2 _ e1 e2    = ExtsExpr $ extsExpr e1 ++ extsExpr e2
  op3 _ e1 e2 e3 = ExtsExpr $ extsExpr e1 ++ extsExpr e2 ++ extsExpr e3

--------------------------------------------------------------------------------

-- Difference lists:

type DList a = [a] -> [a]

empty :: DList a
empty = id

singleton :: a -> DList a
singleton = (:)

(++) :: DList a -> DList a -> DList a
(++) f g = f . g

concat :: [DList a] -> DList a
concat = foldr (.) empty

toList :: DList a -> [a]
toList = ($ [])

--------------------------------------------------------------------------------