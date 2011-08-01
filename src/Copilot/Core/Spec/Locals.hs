--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

-- |

module Copilot.Core.Spec.Locals
  ( Local (..)
  , locals
  ) where

import Copilot.Core
import Data.DList (DList, empty, singleton, append, concat, toList)
import Data.List (nubBy)
import Prelude hiding (concat, foldr)

--------------------------------------------------------------------------------

data Local = forall a . Local
  { localName :: Name
  , localType :: Type a }

instance Show Local where
  show Local { localName = name } = name

--------------------------------------------------------------------------------

locals :: Spec -> [Local]
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

  eqLoc :: Local -> Local -> Bool
  eqLoc Local { localName = name1 } Local { localName = name2 } =
    name1 == name2

--------------------------------------------------------------------------------

locsStream :: Stream -> DList Local
locsStream Stream { streamExpr = e } = locsExpr e

--------------------------------------------------------------------------------

locsTrigger :: Trigger -> DList Local
locsTrigger Trigger { triggerGuard = e, triggerArgs = args } =
  locsExpr e `append` concat (fmap locsUExpr args)

  where

  locsUExpr :: UExpr -> DList Local
  locsUExpr (UExpr _ e1) = locsExpr e1

--------------------------------------------------------------------------------

locsObserver :: Observer -> DList Local
locsObserver Observer { observerExpr = e } = locsExpr e

--------------------------------------------------------------------------------

newtype ExtsExpr a = ExtsExpr { locsExpr :: DList Local }

instance Expr ExtsExpr where
  const  _ _           = ExtsExpr $ empty
  drop   _ _ _         = ExtsExpr $ empty
  local t _ name e1 e2 = ExtsExpr $ singleton (Local name t)
                                      `append` locsExpr e1
                                      `append` locsExpr e2
  var _ _              = ExtsExpr $ empty
  extern _ _           = ExtsExpr $ empty
  op1 _ e              = ExtsExpr $ locsExpr e
  op2 _ e1 e2          = ExtsExpr $ locsExpr e1 `append` locsExpr e2
  op3 _ e1 e2 e3       = ExtsExpr $ locsExpr e1 `append` locsExpr e2
                                                `append` locsExpr e3

--------------------------------------------------------------------------------