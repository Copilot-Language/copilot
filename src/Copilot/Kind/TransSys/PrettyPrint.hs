--------------------------------------------------------------------------------

module Copilot.Kind.TransSys.PrettyPrint ( prettyPrint ) where

import Copilot.Kind.TransSys.Spec

import Text.PrettyPrint.HughesPJ

import qualified Data.Map   as Map
import qualified Data.Bimap as Bimap

--------------------------------------------------------------------------------
  
indent     = nest 4
emptyLine  = text ""

prettyPrint :: Spec -> String
prettyPrint = render . pSpec

pSpec :: Spec -> Doc
pSpec spec = items
  where items = foldr (($$) . pNode) empty (specNodes spec)

pType :: Type t -> Doc
pType = text . show

pList :: (t -> Doc) -> [t] -> Doc
pList f l = brackets (hcat . punctuate (comma <> space) $ map f l)

pNode :: Node -> Doc
pNode n = 
  header $$ imported $$ local $$ emptyLine
  where 
    header =
      text "NODE"
      <+> quotes (text $ nodeId n) 
      <+> text "DEPENDS ON"
      <+> pList (text) (nodeDependencies n)
      
    imported
      | Bimap.null (nodeImportedVars n) = empty
      | otherwise = text "IMPORTS" $$ ( indent $
        Map.foldrWithKey (\k -> ($$) . (pIVar k)) 
        empty (Bimap.toMap $ nodeImportedVars n) )

    local
      | Map.null (nodeLocalVars n) = empty
      | otherwise = text "DEFINES" $$ ( indent $
        Map.foldrWithKey (\k -> ($$) . (pLVar k)) 
        empty (nodeLocalVars n) )
            

        

pConst :: Type t -> t -> Doc
pConst Integer v = text $ show v
pConst Real    v = text $ show v
pConst Bool    v = text $ show v


pIVar :: Var -> ExtVar -> Doc
pIVar v (ExtVar n v') = 
  parens (text n <+> text ":" <+> text (varName v'))
  <+> text "as" <+> quotes (text (varName v))

pLVar :: Var -> LVarDescr -> Doc
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
