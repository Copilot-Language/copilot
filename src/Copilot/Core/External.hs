--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.External
  ( ExternVar (..), ExternArray (..), ExternFun (..)
  , externVars, externArrays, externFuns
  ) where

import Copilot.Core.Expr
import Copilot.Core.Type
import Copilot.Core.Spec
import Data.DList (DList, empty, singleton, append, concat, toList)
import Data.List (nubBy)
import Prelude hiding (all, concat, foldr)

--------------------------------------------------------------------------------

data ExternVar = ExternVar
  { externVarName :: Name
  , externVarType :: UType }

data ExternArray = ExternArray
  { externArrayName :: Name
  , externArrayType :: UType }

data ExternFun = ExternFun
  { externFunName      :: Name
  , externFunType      :: UType
  , externFunArgsTypes :: [UType] }

--------------------------------------------------------------------------------

externVars :: Spec -> [ExternVar]
externVars = nubBy eqExt . toList . all externVarsExpr

  where

  eqExt :: ExternVar -> ExternVar -> Bool
  eqExt ExternVar { externVarName = name1 } ExternVar { externVarName = name2 } =
    name1 == name2

newtype ExternVarsExpr a = ExternVarsExpr { externVarsExpr :: DList ExternVar }

instance Expr ExternVarsExpr where
  const  _ _          = ExternVarsExpr $ empty
  drop   _ _ _        = ExternVarsExpr $ empty
  local _ _ _ e1 e2   = ExternVarsExpr $ externVarsExpr e1 `append`
                                         externVarsExpr e2
  var _ _             = ExternVarsExpr $ empty
  externVar t name    = ExternVarsExpr $ singleton (ExternVar name (UType t))
  externArray _ _ _ e = ExternVarsExpr $ externVarsExpr e
  externFun _ _ ues   = ExternVarsExpr $ concat (map externVarsUExpr ues)
  op1 _ e             = ExternVarsExpr $ externVarsExpr e
  op2 _ e1 e2         = ExternVarsExpr $ externVarsExpr e1 `append`
                                         externVarsExpr e2
  op3 _ e1 e2 e3      = ExternVarsExpr $ externVarsExpr e1 `append`
                                         externVarsExpr e2 `append`
                                         externVarsExpr e3

externVarsUExpr :: UExpr -> DList ExternVar
externVarsUExpr UExpr { uExprExpr = e } = externVarsExpr e

--------------------------------------------------------------------------------

externArrays :: Spec -> [ExternArray]
externArrays = nubBy eqExt . toList . all externArraysExpr

  where

  eqExt :: ExternArray -> ExternArray -> Bool
  eqExt
    ExternArray { externArrayName = name1 }
    ExternArray { externArrayName = name2 } = name1 == name2

newtype ExternArraysExpr a = ExternArraysExpr
  { externArraysExpr :: DList ExternArray }

instance Expr ExternArraysExpr where
  const  _ _               = ExternArraysExpr $ empty
  drop   _ _ _             = ExternArraysExpr $ empty
  local _ _ _ e1 e2        = ExternArraysExpr $ externArraysExpr e1 `append`
                                                externArraysExpr e2
  var _ _                  = ExternArraysExpr $ empty
  externVar _ _            = ExternArraysExpr $ empty
  externArray t1 t2 name e = ExternArraysExpr $ singleton (ExternArray name (UType t1))
  externFun _ _ ues        = ExternArraysExpr $ concat (map externArraysUExpr ues)
  op1 _ e                  = ExternArraysExpr $ externArraysExpr e
  op2 _ e1 e2              = ExternArraysExpr $ externArraysExpr e1 `append`
                                                externArraysExpr e2
  op3 _ e1 e2 e3           = ExternArraysExpr $ externArraysExpr e1 `append`
                                                externArraysExpr e2 `append`
                                                externArraysExpr e3

externArraysUExpr :: UExpr -> DList ExternArray
externArraysUExpr UExpr { uExprExpr = e } = externArraysExpr e

--------------------------------------------------------------------------------

externFuns :: Spec -> [ExternFun]
externFuns _ = []

  where

{-
  eqExt :: ExternVar -> ExternVar -> Bool
  eqExt ExternVar { externVarName = name1 } ExternVar { externVarName = name2 } =
    name1 == name2

newtype ExternVarsExpr a = ExternVarsExpr { externVarsExpr :: DList ExternVar }

instance Expr ExternVarsExpr where
  const  _ _          = ExternVarsExpr $ empty
  drop   _ _ _        = ExternVarsExpr $ empty
  local _ _ _ e1 e2   = ExternVarsExpr $ externVarsExpr e1 `append`
                                         externVarsExpr e2
  var _ _             = ExternVarsExpr $ empty
  extern t name       = ExternVarsExpr $ singleton (ExternVar name (utype t))
  externArray _ _ _ e = ExternVarsExpr $ externVarsExpr e
  externFun _ _ ues   = ExternVarsExpr $ concat (map externVarsUExpr ues)
  op1 _ e             = ExternVarsExpr $ externVarsExpr e
  op2 _ e1 e2         = ExternVarsExpr $ externVarsExpr e1 `append`
                                         externVarsExpr e2
  op3 _ e1 e2 e3      = ExternVarsExpr $ externVarsExpr e1 `append`
                                         externVarsExpr e2 `append`
                                         externVarsExpr e3

externVarsUExpr :: UExpr -> DList ExternVar
externVarsUExpr UExpr { uExprExpr = e } = externVarsExpr e
-}

--------------------------------------------------------------------------------

all :: Expr e => (forall a . e a -> DList b) -> Spec -> DList b
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
