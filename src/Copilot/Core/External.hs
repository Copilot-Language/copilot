--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.External
  ( ExtVar (..), ExtArray (..), ExtFun (..)
  , externVars, externArrays, externFuns
  ) where

import Copilot.Core.Expr
import Copilot.Core.Type
import Copilot.Core.Spec
import Data.DList (DList, empty, singleton, append, concat, toList)
import Data.List (nubBy)
import Prelude hiding (all, concat, foldr)

--------------------------------------------------------------------------------

data ExtVar = ExtVar
  { externVarName :: Name
  , externVarType :: UType }

data ExtArray = forall a b . Integral a => ExtArray
  { externArrayName     :: Name
  , externArrayElemType :: Type b
  , externArrayIdx      :: Expr a
  , externArrayIdxType  :: Type a
  , externArraySize     :: Int
  , externArrayTag      :: Maybe Tag }

data ExtFun = forall a . ExtFun
  { externFunName      :: Name
  , externFunType      :: Type a
  , externFunArgs      :: [UExpr]
  , externFunTag       :: Maybe Tag }

--------------------------------------------------------------------------------

externVars :: Spec -> [ExtVar]
externVars = nubBy eqExt . toList . all externVarsExpr
  where
  eqExt :: ExtVar -> ExtVar -> Bool
  eqExt ExtVar { externVarName = name1 } ExtVar { externVarName = name2 } =
    name1 == name2

externVarsExpr :: Expr a -> DList ExtVar
externVarsExpr e0 = case e0 of
  Const  _ _                -> empty
  Drop   _ _ _              -> empty
  Local _ _ _ e1 e2         -> externVarsExpr e1 `append` externVarsExpr e2
  Var _ _                   -> empty
  ExternVar t name _        -> singleton (ExtVar name (UType t))
  ExternArray _ _ _ _ e _ _ -> externVarsExpr e
  ExternFun _ _ ues _ _     -> concat (map externVarsUExpr ues)
  Op1 _ e                   -> externVarsExpr e
  Op2 _ e1 e2               -> externVarsExpr e1 `append` externVarsExpr e2
  Op3 _ e1 e2 e3            -> externVarsExpr e1 `append`
                               externVarsExpr e2 `append`
                               externVarsExpr e3

externVarsUExpr :: UExpr -> DList ExtVar
externVarsUExpr UExpr { uExprExpr = e } = externVarsExpr e

--------------------------------------------------------------------------------

externArrays :: Spec -> [ExtArray]
externArrays = toList . all externArraysExpr

externArraysExpr :: Expr a -> DList ExtArray
externArraysExpr e0 = case e0 of
  Const  _ _                      -> empty
  Drop   _ _ _                    -> empty
  Local _ _ _ e1 e2               -> externArraysExpr e1 `append` externArraysExpr e2
  Var _ _                         -> empty
  ExternVar _ _ _                 -> empty
  ExternArray t1 t2  name 
              size idx _ tag      -> singleton (ExtArray name t2 idx t1 size tag)
  ExternFun _ _ ues _ _           -> concat (map externArraysUExpr ues)
  Op1 _ e                         -> externArraysExpr e
  Op2 _ e1 e2                     -> externArraysExpr e1 `append` externArraysExpr e2
  Op3 _ e1 e2 e3                  -> externArraysExpr e1 `append`
                                     externArraysExpr e2 `append`
                                     externArraysExpr e3

externArraysUExpr :: UExpr -> DList ExtArray
externArraysUExpr UExpr { uExprExpr = e } = externArraysExpr e

--------------------------------------------------------------------------------

externFuns :: Spec -> [ExtFun]
externFuns = toList . all externFunsExpr

externFunsExpr :: Expr a -> DList ExtFun
externFunsExpr e0 = case e0 of
  Const  _ _                  -> empty
  Drop   _ _ _                -> empty
  Local _ _ _ e1 e2           -> externFunsExpr e1 `append` externFunsExpr e2
  Var _ _                     -> empty
  ExternVar _ _ _             -> empty
  ExternArray _ _ _ _ idx _ _ -> externFunsExpr idx
  ExternFun t name ues _ tag  -> singleton (ExtFun name t ues tag)
  Op1 _ e                     -> externFunsExpr e
  Op2 _ e1 e2                 -> externFunsExpr e1 `append` externFunsExpr e2
  Op3 _ e1 e2 e3              -> externFunsExpr e1 `append`
                                 externFunsExpr e2 `append`
                                 externFunsExpr e3

--------------------------------------------------------------------------------

all :: (forall a . Expr a -> DList b) -> Spec -> DList b
all f spec =
  concat (fmap (allStream) (specStreams   spec)) `append`
  concat (fmap allTrigger  (specTriggers  spec)) `append`
  concat (fmap allObserver (specObservers spec))

  where
  
  allStream
    Stream { streamExpr = e } = f e

  allTrigger
    Trigger
      { triggerGuard = e
      , triggerArgs = args } = f e `append` concat (fmap allUExpr args)

  allUExpr
    (UExpr _ e1) = f e1

  allObserver
    Observer { observerExpr = e } = f e
