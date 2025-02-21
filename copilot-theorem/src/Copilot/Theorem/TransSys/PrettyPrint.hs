{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Safe           #-}

-- | Pretty print a TransSys specification as a Kind2/Lustre specification.
module Copilot.Theorem.TransSys.PrettyPrint ( prettyPrint ) where

import Copilot.Theorem.TransSys.Spec

import Text.PrettyPrint.HughesPJ

import qualified Data.Map   as Map
import qualified Data.Bimap as Bimap

import Prelude hiding ((<>))

indent     = nest 4
emptyLine  = text ""

-- | Pretty print a TransSys specification as a Kind2/Lustre specification.
prettyPrint :: TransSys -> String
prettyPrint = render . pSpec

pSpec :: TransSys -> Doc
pSpec spec = items $$ props
  where
    items = foldr (($$) . pNode) empty (specNodes spec)
    props = text "PROPS" $$
      Map.foldrWithKey (\k -> ($$) . pProp k)
        empty
        (Map.map fst (specProps spec))

pProp pId extvar = quotes (text pId) <+> text "is" <+> pExtVar extvar

pType :: Type t -> Doc
pType = text . show

pList :: (t -> Doc) -> [t] -> Doc
pList f l = brackets (hcat . punctuate (comma <> space) $ map f l)

pNode :: Node -> Doc
pNode n =
  header $$ imported $$ local $$ constrs $$ emptyLine
  where
    header =
      text "NODE"
      <+> quotes (text $ nodeId n)
      <+> text "DEPENDS ON"
      <+> pList text (nodeDependencies n)

    imported
      | Bimap.null (nodeImportedVars n) = empty
      | otherwise = text "IMPORTS" $$ indent
        (Map.foldrWithKey (\k -> ($$) . pIVar k)
        empty (Bimap.toMap $ nodeImportedVars n))

    local
      | Map.null (nodeLocalVars n) = empty
      | otherwise = text "DEFINES" $$ indent
        (Map.foldrWithKey (\k -> ($$) . pLVar k)
        empty (nodeLocalVars n))

    constrs = case nodeConstrs n of
      [] -> empty
      l  -> text "WITH CONSTRAINTS" $$
            foldr (($$) . pExpr) empty l

pConst :: Type t -> t -> Doc
pConst Integer v = text $ show v
pConst Real    v = text $ show v
pConst Bool    v = text $ show v

pExtVar :: ExtVar -> Doc
pExtVar (ExtVar n v) = parens (text n <+> text ":" <+> text (varName v))

pIVar :: Var -> ExtVar -> Doc
pIVar v ev =
  pExtVar ev
  <+> text "as" <+> quotes (text (varName v))

pLVar :: Var -> VarDescr -> Doc
pLVar l (VarDescr {varType, varDef}) = header $$ indent body
  where
    header =
      text (varName l)
      <+> text ":"
      <+> pType varType
      <+> text "="

    body = case varDef of
      Pre val var ->
        pConst varType val
        <+> text "->" <+> text "pre"
        <+> text (varName var)
      Expr e -> pExpr e

      Constrs cs ->
        text "{"
        <+> (hsep . punctuate (space <> text ";" <> space)) (map pExpr cs)
        <+> text "}"

pExpr :: Expr t -> Doc

pExpr (Const t v) = pConst t v

pExpr (Ite _ c e1 e2) =
  text "if" <+> pExpr c
  <+> text "then" <+> pExpr e1
  <+> text "else" <+> pExpr e2

pExpr (Op1 _ op e) = pOp1 op <+> parens (pExpr e)

pExpr (Op2 _ op e1 e2) =
  parens (pExpr e1) <+> pOp2 op <+> parens (pExpr e2)

pExpr (VarE _ v) = text (varName v)

pOp1 :: Op1 a -> Doc
pOp1 = text . show

pOp2 :: Op2 a b -> Doc
pOp2 = text . show
