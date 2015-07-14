--------------------------------------------------------------------------------

module Copilot.Kind.Kind2.PrettyPrint ( prettyPrint ) where

import Copilot.Kind.Misc.SExpr
import qualified Copilot.Kind.Misc.SExpr as SExpr
import Copilot.Kind.Kind2.AST

import Data.List (intercalate)

--------------------------------------------------------------------------------

type SSExpr = SExpr String

kwPrime = "prime"

--------------------------------------------------------------------------------

prettyPrint :: File -> String
prettyPrint =
  intercalate "\n\n"
  . map (SExpr.toString shouldIndent id)
  . ppFile

-- Defines the indentation policy of the S-Expressions
shouldIndent :: SSExpr -> Bool
shouldIndent (Atom _)                   = False
shouldIndent (List [Atom a, Atom _])    = a `notElem` [kwPrime]
shouldIndent _                          = True

--------------------------------------------------------------------------------

ppFile :: File -> [SSExpr]
ppFile (File preds props) = map ppPredDef preds ++ ppProps props

ppProps :: [Prop] -> [SSExpr]
ppProps ps = [ node "check-prop" [ list $ map ppProp ps ] ]

ppProp :: Prop -> SSExpr
ppProp (Prop n t) = list [atom n, ppTerm t]

ppPredDef :: PredDef -> SSExpr
ppPredDef pd =
  list [ atom "define-pred"
       , atom (predId pd)
       , list . map ppStateVarDef . predStateVars $ pd
       , node "init"  [ppTerm $ predInit  pd]
       , node "trans" [ppTerm $ predTrans pd] ]

ppStateVarDef :: StateVarDef -> SSExpr
ppStateVarDef svd =
  list [atom (varId svd), ppType (varType svd)]

ppType :: Type -> SSExpr
ppType Int  = atom "Int"
ppType Real = atom "Real"
ppType Bool = atom "Bool"

ppTerm :: Term -> SSExpr
ppTerm (ValueLiteral  c) = atom c
ppTerm (PrimedStateVar v) = list [atom kwPrime, atom v]
ppTerm (StateVar v) = atom v
ppTerm (FunApp f args) = node f $ map ppTerm args
ppTerm (PredApp p t args) = node (p ++ "." ++ ext) $ map ppTerm args
  where ext = case t of
         Init -> "init"
         Trans -> "trans"

--------------------------------------------------------------------------------
