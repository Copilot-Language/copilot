--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Let expressions.
--
-- Although Copilot is a DSL embedded in Haskell and Haskell does support let
-- expressions, we want Copilot to be able to implement explicit sharing, to
-- detect when the same stream is being used in multiple places in a
-- specification and avoid recomputing it unnecessarily.

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Core.Locals
  {-# DEPRECATED "This module is deprecated." #-}
  ( Loc (..)
  , locals
  ) where

import Copilot.Core hiding (toList)
import Data.DList (DList, empty, singleton, append, concat, toList)
import Data.List (nubBy)
import Prelude hiding (concat, foldr)

--------------------------------------------------------------------------------

-- | A local definition, with a given stream name and stream type.
data Loc = forall a . Loc
  { localName :: Name
  , localType :: Type a }

-- | Show the underlying stream name.
instance Show Loc where
  show Loc { localName = name } = name

--------------------------------------------------------------------------------

-- | Obtain all the local definitions in a specification.
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

-- | Obtain all the local definitions in a stream.
locsStream :: Stream -> DList Loc
locsStream Stream { streamExpr = e } = locsExpr e

--------------------------------------------------------------------------------

-- | Obtain all the local definitions in a trigger.
locsTrigger :: Trigger -> DList Loc
locsTrigger Trigger { triggerGuard = e, triggerArgs = args } =
  locsExpr e `append` concat (fmap locsUExpr args)

  where

  locsUExpr :: UExpr -> DList Loc
  locsUExpr (UExpr _ e1) = locsExpr e1

--------------------------------------------------------------------------------

-- | Obtain all the local definitions in an observer.
locsObserver :: Observer -> DList Loc
locsObserver Observer { observerExpr = e } = locsExpr e

--------------------------------------------------------------------------------

-- | Obtain all the local definitions in an expression.
locsExpr :: Expr a -> DList Loc
locsExpr e0 = case e0 of
  Const  _ _             -> empty
  Drop   _ _ _           -> empty
  Local t _ name e1 e2   -> singleton (Loc name t)
                                        `append` locsExpr e1
                                        `append` locsExpr e2
  Var _ _                    -> empty
  ExternVar _ _ _            -> empty
  Op1 _ e                    -> locsExpr e
  Op2 _ e1 e2                -> locsExpr e1 `append` locsExpr e2
  Op3 _ e1 e2 e3             -> locsExpr e1 `append` locsExpr e2
                                            `append` locsExpr e3
  Label _ _ e                -> locsExpr e

--------------------------------------------------------------------------------
