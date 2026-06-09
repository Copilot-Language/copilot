{-# LANGUAGE Safe #-}

-- | Pretty print a Kind2 file defining nodes and propositions, in the native
-- transition system input format supported by Kind2 1.0 and newer (see
-- "Copilot.Theorem.Kind2.AST" for pointers to references on the format).
module Copilot.Theorem.Kind2.PrettyPrint ( prettyPrint ) where

import Copilot.Theorem.Misc.SExpr
import qualified Copilot.Theorem.Misc.SExpr as SExpr
import Copilot.Theorem.Kind2.AST

import Data.List (intercalate)

-- | A tree of expressions, in which the leafs are strings.
type SSExpr = SExpr String

-- | Reserved keyword prime.
kwPrime = "prime"

-- | Dummy position attached to the properties of the file.
--
-- Kind2 requires a position (in the format @file:row-col@) for properties
-- declared with the @:user@ source annotation, and reports that position back
-- in its output.
propPosition :: String
propPosition = "copilot:1-1"

-- | Pretty print a Kind2 file.
prettyPrint :: File -> String
prettyPrint =
  intercalate "\n\n"
  . map (SExpr.toString shouldIndent id)
  . ppFile

-- | Define the indentation policy of the S-Expressions
shouldIndent :: SSExpr -> Bool
shouldIndent (Atom _)                   = False
shouldIndent (List [Atom a, Atom _])    = a `notElem` [kwPrime]
shouldIndent _                          = True

-- | Convert a file into a sequence of expressions.
--
-- The top node is printed last, since Kind2 analyzes the last node of the
-- file as the top system. The properties of the file are attached to it.
ppFile :: File -> [SSExpr]
ppFile (File nodes top props) =
  map (`ppNode` []) nodes ++ [ppNode top (ppProps props)]

-- | Convert a sequence of propositions into a props field.
ppProps :: [Prop] -> [SSExpr]
ppProps [] = []
ppProps ps = [ node "props" [ list $ map ppProp ps ] ]

-- | Convert a proposition into an expression.
ppProp :: Prop -> SSExpr
ppProp (Prop n t) = list [atom n, ppTerm t, atom ":user", atom propPosition]

-- | Convert a node, together with optional extra fields, into an expression.
ppNode :: Node -> [SSExpr] -> SSExpr
ppNode n extraFields =
  list $ [ atom "define-node"
         , atom (nodeId n)
         , list . map ppStateVarDef . nodeStateVars $ n
         , node "init"  [ppTerm $ nodeInit  n]
         , node "trans" [ppTerm $ nodeTrans n] ]
         ++ extraFields

-- | Convert a state variable definition into an expression.
ppStateVarDef :: StateVarDef -> SSExpr
ppStateVarDef svd =
  list $ [atom (varId svd), ppType (varType svd)]
         ++ map ppStateVarFlag (varFlags svd)

-- | Convert a state variable option into an expression.
ppStateVarFlag :: StateVarFlag -> SSExpr
ppStateVarFlag FConst = atom ":const"

-- | Convert a type into an expression.
ppType :: Type -> SSExpr
ppType Int  = atom "Int"
ppType Real = atom "Real"
ppType Bool = atom "Bool"

-- | Convert a term into an expression.
ppTerm :: Term -> SSExpr
ppTerm (ValueLiteral  c) = atom c
ppTerm (PrimedStateVar v) = list [atom kwPrime, atom v]
ppTerm (StateVar v) = atom v
ppTerm (FunApp f args) = node f $ map ppTerm args
ppTerm (PredApp p t args) = node (p ++ "." ++ ext) $ map ppTerm args
  where
    ext = case t of
      Init -> "init"
      Trans -> "trans"
