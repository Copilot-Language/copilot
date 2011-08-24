--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

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

data ExtArray = ExtArray
  { externArrayName :: Name
  , externArrayType :: UType }

data ExtFun = ExtFun
  { externFunName      :: Name
  , externFunType      :: UType
  , externFunArgsTypes :: [UType] }

--------------------------------------------------------------------------------

externVars :: Spec -> [ExtVar]
externVars = nubBy eqExt . toList . all externVarsExpr

  where

  eqExt :: ExtVar -> ExtVar -> Bool
  eqExt ExtVar { externVarName = name1 } ExtVar { externVarName = name2 } =
    name1 == name2

externVarsExpr :: Expr a -> DList ExtVar
externVarsExpr e0 = case e0 of
  Const  _ _          -> empty
  Drop   _ _ _        -> empty
  Local _ _ _ e1 e2   -> externVarsExpr e1 `append`
                                         externVarsExpr e2
  Var _ _             -> empty
  ExternVar t name    -> singleton (ExtVar name (UType t))
  ExternArray _ _ _ e -> externVarsExpr e
  ExternFun _ _ ues   -> concat (map externVarsUExpr ues)
  Op1 _ e             -> externVarsExpr e
  Op2 _ e1 e2         -> externVarsExpr e1 `append` externVarsExpr e2
  Op3 _ e1 e2 e3      -> externVarsExpr e1 `append`
                         externVarsExpr e2 `append`
                         externVarsExpr e3

externVarsUExpr :: UExpr -> DList ExtVar
externVarsUExpr UExpr { uExprExpr = e } = externVarsExpr e

--------------------------------------------------------------------------------

externArrays :: Spec -> [ExtArray]
externArrays = nubBy eqExt . toList . all externArraysExpr

  where

  eqExt :: ExtArray -> ExtArray -> Bool
  eqExt
    ExtArray { externArrayName = name1 }
    ExtArray { externArrayName = name2 } = name1 == name2

externArraysExpr :: Expr a -> DList ExtArray
externArraysExpr e0 = case e0 of
  Const  _ _               -> empty
  Drop   _ _ _             -> empty
  Local _ _ _ e1 e2        -> externArraysExpr e1 `append`
                                                externArraysExpr e2
  Var _ _                  -> empty
  ExternVar _ _            -> empty
  ExternArray t1 _  name _ -> singleton (ExtArray name (UType t1))
  ExternFun _ _ ues        -> concat (map externArraysUExpr ues)
  Op1 _ e                  -> externArraysExpr e
  Op2 _ e1 e2              -> externArraysExpr e1 `append` externArraysExpr e2
  Op3 _ e1 e2 e3           -> externArraysExpr e1 `append`
                              externArraysExpr e2 `append`
                              externArraysExpr e3

externArraysUExpr :: UExpr -> DList ExtArray
externArraysUExpr UExpr { uExprExpr = e } = externArraysExpr e

--------------------------------------------------------------------------------

externFuns :: Spec -> [ExtFun]
externFuns _ = []

  where

{-
  eqExt :: ExtVar -> ExtVar -> Bool
  eqExt ExtVar { externVarName = name1 } ExtVar { externVarName = name2 } =
    name1 == name2

newtype ExtVarsExpr a = ExtVarsExpr { externVarsExpr :: DList ExtVar }

instance Expr ExtVarsExpr where
  const  _ _          -> empty
  drop   _ _ _        -> empty
  local _ _ _ e1 e2   -> externVarsExpr e1 `append`
                                         externVarsExpr e2
  var _ _             -> empty
  extern t name       -> singleton (ExtVar name (utype t))
  externArray _ _ _ e -> externVarsExpr e
  externFun _ _ ues   -> concat (map externVarsUExpr ues)
  op1 _ e             -> externVarsExpr e
  op2 _ e1 e2         -> externVarsExpr e1 `append`
                                         externVarsExpr e2
  op3 _ e1 e2 e3      -> externVarsExpr e1 `append`
                                         externVarsExpr e2 `append`
                                         externVarsExpr e3

externVarsUExpr :: UExpr -> DList ExtVar
externVarsUExpr UExpr { uExprExpr = e } = externVarsExpr e
-}

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
