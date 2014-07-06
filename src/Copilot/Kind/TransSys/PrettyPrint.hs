--------------------------------------------------------------------------------

module Copilot.Kind.TransSys.PrettyPrint ( prettyPrint ) where

import Copilot.Kind.TransSys.Spec

import Text.PrettyPrint.HughesPJ

import qualified Data.Map as Map

--------------------------------------------------------------------------------
  
indent     = nest 4
emptyLine  = text ""

prettyPrint :: Spec -> String
prettyPrint = render . pSpec

pSpec :: Spec -> Doc
pSpec spec = items
  where items =
          text "MODEL"
          $$ indent (foldr (($$) . pNode) empty (specNodes spec))

pType :: Type t -> Doc
pType = text . show

pList :: (t -> Doc) -> [t] -> Doc
pList f l = brackets (hcat . punctuate (comma <> space) $ map f l)

pNode :: Node -> Doc
pNode n = header $$ indent body $$ emptyLine

  where header =
          text "NODE"
          <+> quotes (text $ nodeId n) 
          <+> text "DEPENDS"
          <+> pList (text) (nodeDependencies n)

        body = Map.foldrWithKey (\k -> ($$) . (pLVar k)) empty (nodeVars n)

pConst :: Type t -> t -> Doc
pConst Integer v = text $ show v
pConst Bool    v = text $ show v

pLVar :: LVar -> LVarDescr -> Doc
pLVar l (LVarDescr {varType, varDef}) = header $$ indent body
  where header =
          text (varName l)
          <+> text ":"
          <+> pType varType
          <+> text "="
          
        body = case varDef of
          Pre val var ->
            pConst varType val
            <+> text "->" <+> text "pre"
            <+> text (varName var)
          Ext v ->
            text "ext"
            <>  brackets (text (varNode v))
            <+> quotes (text (varName v))
          Expr e -> pExpr e


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

pOp1 :: Op1 a b -> Doc
pOp1 = text . show

pOp2 :: Op2 a b c -> Doc
pOp2 = text . show

--------------------------------------------------------------------------------
