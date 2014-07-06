--------------------------------------------------------------------------------

module Copilot.Kind.Kind2Format where

import Copilot.Kind.TransSys
import Copilot.Kind.Misc.SExpr
import Copilot.Kind.Misc.Utils


import qualified Data.Map as Map


--------------------------------------------------------------------------------

type SSExpr   = SExpr String
type K2Output = [SSExpr]

--------------------------------------------------------------------------------

-- | The following properties MUST hold for the given transition system :
-- | * Nodes are sorted by topological order
-- | * Nodes are `completed`, which means that each node imports all the
-- |   variables of its dependencies
-- | * ---

--------------------------------------------------------------------------------

-- Keyword used to select the primed version of a variable
kwPrime :: String
kwPrime = "prime"

andNode :: [SSExpr] -> SSExpr
andNode = node "and"

--------------------------------------------------------------------------------

-- Defines the indentation policy of the S-Expressions
shouldIndent :: SSExpr -> Bool
shouldIndent (Atom _)                       = False
shouldIndent (List [Atom a, Atom _])        = not $ a `elem` [kwPrime]
shouldIndent _                              = True

toKind2 :: Spec -> String
toKind2 = intercalate "\n\n"
          . map (toString shouldIndent id) . trSpec
          . complete . removeCycles

--------------------------------------------------------------------------------

trSpec :: Spec -> K2Output
trSpec spec = map trNode (specNodes spec)
              ++ trProps (specProps spec) (specAssertDeps spec)


trProps :: Map PropId GVar -> Map PropId [PropId] -> [SSExpr]
trProps props assertDeps = [ node "check-prop" $ [ list $
  map trProp $ Map.keys assertDeps ] ]
  where
    trProp pid =
      list [atom pid, trLVar $ localPart $ props ! pid]

trNode :: Node -> SSExpr
trNode n = list [ atom "define-pred"
                , atom (nodeId n)
                , list vdefs
                , node "init"  [andNode init]
                , node "trans" [andNode trans] ]
  where

    -- The list of the local variables : ensure this list is
    -- sorted with the same order used by 'sortedlocalAliases'
    
    vdefs = Map.elems $ Map.mapWithKey vdef (nodeVars n)

    vdef var (LVarDescr t _) = (list [atom $ varName var, atom $ show t])

    slas = sortedlocalAliases n

    init  = (trLocals trVarDescrInit  n) ++ (trExtsInit  slas)
    trans = (trLocals trVarDescrTrans n) ++ (trExtsTrans slas)

--------------------------------------------------------------------------------

trLocals :: (LVar -> LVarDescr -> Maybe SSExpr) -> Node -> [SSExpr]
trLocals trVarDescr =
  catMaybes . Map.elems . Map.mapWithKey trVarDescr . nodeVars
  

trVarDescrInit :: LVar -> LVarDescr -> Maybe SSExpr
trVarDescrInit v (LVarDescr t def) =
  trDef t def >>= \rhs -> return $ node "=" [trLVar v, rhs]
  where
    trDef :: forall t . Type t -> LVarDef t -> Maybe SSExpr
    trDef t (Pre v _) = return $ trConst t v
    trDef _ (Expr e)  = return $ trExpr False e
    trDef _ (Ext _)   = Nothing


trVarDescrTrans :: LVar -> LVarDescr -> Maybe  SSExpr
trVarDescrTrans v (LVarDescr t def) =
  trDef t def >>= \rhs -> return $ node "=" [trPrimedLVar v, rhs]
  where
    trDef :: forall t . Type t -> LVarDef t -> Maybe SSExpr
    trDef _ (Pre _ v) = return $ trLVar v
    trDef _ (Expr e)  = return $ trExpr True e
    trDef _ (Ext _)   = Nothing

--------------------------------------------------------------------------------

-- Returns a list of of pairs '(nodeId, lvars)' where : 
-- 'lvars' is a list of the local aliases associated to 'nodeID',
-- sorted alphabetically by the name of the variable being matched

sortedlocalAliases :: Node -> [(NodeId, [LVar])]
sortedlocalAliases n =
  map (\l -> (fst3 . head $ l, map thrd3 l))
  . groupBy ((==) `on` fst3)
  . sortBy lexicord
  $ extVars
  where
    lexicord x y = (compare `on` fst3) x y <> (compare `on` snd3) x y

    extVars :: [(NodeId, LVar, LVar)]
    extVars =  do
      (alias, LVarDescr _ (Ext (GVar node var))) <- Map.toList (nodeVars n)
      return (node, var, alias)


trExtsTrans :: [(NodeId, [LVar])] -> [SSExpr]
trExtsTrans slas = map transPred slas
  where
    transPred (nodeId, vars) =
      list $ [ atom (nodeId ++ ".trans") ]
             ++ map trLVar vars
             ++ map trPrimedLVar vars


trExtsInit :: [(NodeId, [LVar])] -> [SSExpr]
trExtsInit slas = map initPred slas
  where
    initPred (nodeId, vars) =
      list $ [ atom (nodeId ++ ".init") ]
             ++ map trLVar vars
  
--------------------------------------------------------------------------------

trConst :: Type t -> t -> SSExpr
trConst Integer v     = atom $ show v
trConst Bool    True  = atom $ "true"
trConst Bool    False = atom $ "false"


trLVar :: LVar -> SSExpr
trLVar = atom . varName

trPrimedLVar :: LVar -> SSExpr
trPrimedLVar v = list [atom kwPrime, trLVar v]

trExpr :: Bool -> Expr t -> SSExpr
trExpr primed = tr
  where
    tr :: forall t . Expr t -> SSExpr
    tr (Const t c) = trConst t c
    tr (Ite _ c e1 e2) = node "ite" [tr c, tr e1, tr e2]
    tr (Op1 _ op e) = node (show op) [tr e]
    tr (Op2 _ op e1 e2) = node (show op) [tr e1, tr e2]
    tr (VarE _ v) = if primed then trPrimedLVar v else trLVar v

--------------------------------------------------------------------------------
