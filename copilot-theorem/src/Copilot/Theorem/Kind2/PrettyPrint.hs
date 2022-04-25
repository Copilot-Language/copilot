{-# LANGUAGE Safe #-}

-- | Pretty print a Kind2 file defining predicates and propositions.
module Copilot.Theorem.Kind2.PrettyPrint ( prettyPrint ) where

import Copilot.Theorem.Misc.SExpr
import qualified Copilot.Theorem.Misc.SExpr as SExpr
import Copilot.Theorem.Kind2.AST

import Data.List (intercalate)

-- | A tree of expressions, in which the leafs are strings.
type SSExpr = SExpr String

-- | Reserved keyword prime.
kwPrime = "prime"

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
ppFile :: File -> [SSExpr]
ppFile (File preds props) = map ppPredDef preds ++ ppProps props

-- | Convert a sequence of propositions into command to check each of them.
ppProps :: [Prop] -> [SSExpr]
ppProps ps = [ node "check-prop" [ list $ map ppProp ps ] ]

-- | Convert a proposition into an expression.
ppProp :: Prop -> SSExpr
ppProp (Prop n t) = list [atom n, ppTerm t]

-- | Convert a predicate into an expression.
ppPredDef :: PredDef -> SSExpr
ppPredDef pd =
  list [ atom "define-pred"
       , atom (predId pd)
       , list . map ppStateVarDef . predStateVars $ pd
       , node "init"  [ppTerm $ predInit  pd]
       , node "trans" [ppTerm $ predTrans pd] ]

-- | Convert a state variable definition into an expression.
ppStateVarDef :: StateVarDef -> SSExpr
ppStateVarDef svd =
  list [atom (varId svd), ppType (varType svd)]

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
  where ext = case t of
         Init -> "init"
         Trans -> "trans"
