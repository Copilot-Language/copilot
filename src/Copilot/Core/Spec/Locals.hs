--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

-- |

module Copilot.Core.Spec.Locals
  ( Loc (..)
  , locals
  ) where

import Copilot.Core
import Data.DList (DList, empty, singleton, append, concat, toList)
import Data.List (nubBy)
import Prelude hiding (concat, foldr)

--------------------------------------------------------------------------------

data Loc = forall a . Loc
  { localName :: Name
  , localType :: Type a }

instance Show Loc where
  show Loc { localName = name } = name

--------------------------------------------------------------------------------

locals :: Spec -> [Loc]
locals
  Spec
    { specStreams   = streams
    , specTriggers  = triggers
    , specObservers = observers
    } = nubBy eqLoc . toList $
          concat (fmap locsStream   streams)  `append`
          concat (fmap locsTrigger  triggers) `append`
          concat (fmap locsObserver observers)

  where

  eqLoc :: Loc -> Loc -> Bool
  eqLoc Loc { localName = name1 } Loc { localName = name2 } =
    name1 == name2

--------------------------------------------------------------------------------

locsStream :: Stream -> DList Loc
locsStream Stream { streamExpr = e } = locsExpr e

--------------------------------------------------------------------------------

locsTrigger :: Trigger -> DList Loc
locsTrigger Trigger { triggerGuard = e, triggerArgs = args } =
  locsExpr e `append` concat (fmap locsUExpr args)

  where

  locsUExpr :: UExpr -> DList Loc
  locsUExpr (UExpr _ e1) = locsExpr e1

--------------------------------------------------------------------------------

locsObserver :: Observer -> DList Loc
locsObserver Observer { observerExpr = e } = locsExpr e

--------------------------------------------------------------------------------

locsExpr :: Expr a -> DList Loc
locsExpr e0 = case e0 of
  Const  _ _             -> empty
  Drop   _ _ _           -> empty
  Local t _ name e1 e2   -> singleton (Loc name t)
                                        `append` locsExpr e1
                                        `append` locsExpr e2
  Var _ _                -> empty
  ExternVar _ _          -> empty
  ExternFun _ _ _ _      -> empty
  ExternArray _ _ _ _ _  -> empty
  Op1 _ e                -> locsExpr e
  Op2 _ e1 e2            -> locsExpr e1 `append` locsExpr e2
  Op3 _ e1 e2 e3         -> locsExpr e1 `append` locsExpr e2
                                        `append` locsExpr e3

--------------------------------------------------------------------------------
