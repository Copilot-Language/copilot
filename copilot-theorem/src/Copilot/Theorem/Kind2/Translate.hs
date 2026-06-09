{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe       #-}

-- | Convert modular transition systems ('TransSys') into Kind2 file
-- specifications.
module Copilot.Theorem.Kind2.Translate
  ( toKind2
  ) where

import Copilot.Theorem.TransSys
import qualified Copilot.Theorem.Kind2.AST as K

import Data.Function (on)
import Data.Maybe (fromJust)

import Data.List (partition, sort, sortBy)
import Data.Map ((!))

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap

-- | Produce a Kind2 file that checks the properties specified.
toKind2 :: [PropId]  -- ^ Assumptions
        -> [PropId]  -- ^ Properties to be checked
        -> TransSys  -- ^ Modular transition system holding the system spec
        -> K.File
toKind2 assumptions checkedProps spec =
  addAssumptions spec' assumptions $ trSpec spec' checkedProps
  where
    spec' = inline spec

trSpec :: TransSys -> [PropId] -> K.File
trSpec spec checkedProps = K.File otherNodes topNode props
  where
    (topNode, otherNodes) =
      case partition ((== specTopNodeId spec) . K.nodeId) nodes of
        ([top], others) -> (top, others)
        _ -> error $ "Kind2.Translate: top node "
                       ++ specTopNodeId spec ++ " not found"
    nodes = map (trNode spec) (specNodes spec)
    props = map trProp $
      filter ((`elem` checkedProps) . fst) $
        Map.toList $ Map.map fst $ specProps spec

trProp :: (PropId, ExtVar) -> K.Prop
trProp (pId, var) = K.Prop pId (trVar . extVarLocalPart $ var)

trNode :: TransSys -> Node -> K.Node
trNode spec node = K.Node
  { K.nodeId        = nodeId node
  , K.nodeStateVars = gatherPredStateVars spec node
  , K.nodeInit      = mkConj $ initLocals  node
                               ++ map (trExpr False) (nodeConstrs node)
  , K.nodeTrans     = mkConj $ transLocals node
                               ++ map (trExpr True) (nodeConstrs node)
  }

-- | Add the assumptions to the top node of the file by conjoining the
-- corresponding property variables to its initial state and transition
-- relation predicates.
addAssumptions :: TransSys -> [PropId] -> K.File -> K.File
addAssumptions spec assumptions file =
  file { K.fileTopNode = aux (K.fileTopNode file) }
  where
    aux node =
      let init'  = mkConj ( K.nodeInit  node : map K.StateVar vars )
          trans' = mkConj ( K.nodeTrans node : map K.PrimedStateVar vars )
      in node { K.nodeInit = init', K.nodeTrans = trans' }

    toExtVar a = fst $ fromJust $ Map.lookup a $ specProps spec
    vars = map (varName . extVarLocalPart . toExtVar) assumptions

-- The ordering really matters here because the variables
-- have to be given in this order in a pred call
-- Our convention :
-- * First the local variables, sorted by alphabetical order
-- * Then the imported variables, by alphabetical order on
--   the father node then by alphabetical order on the variable name

gatherPredStateVars :: TransSys -> Node -> [K.StateVarDef]
gatherPredStateVars spec node = locals ++ imported
  where
    nodesMap = Map.fromList [(nodeId n, n) | n <- specNodes spec]
    extVarType :: ExtVar -> K.Type
    extVarType (ExtVar n v) =
      case nodeLocalVars (nodesMap ! n) ! v of
        VarDescr Integer _ -> K.Int
        VarDescr Bool    _ -> K.Bool
        VarDescr Real    _ -> K.Real

    locals =
      map (\v -> K.StateVarDef (varName v)
              (extVarType $ ExtVar (nodeId node) v) [])
         . sort . Map.keys $ nodeLocalVars node

    imported =
      map (\(v, ev) -> K.StateVarDef (varName v) (extVarType ev) [])
      . sortBy (compare `on` snd) . Bimap.toList $ nodeImportedVars node

mkConj :: [K.Term] -> K.Term
mkConj []  = trConst Bool True
mkConj [x] = x
mkConj xs  = K.FunApp "and" xs

mkEquality :: K.Term -> K.Term -> K.Term
mkEquality t1 t2 = K.FunApp "=" [t1, t2]

trVar :: Var -> K.Term
trVar v = K.StateVar (varName v)

trPrimedVar :: Var -> K.Term
trPrimedVar v = K.PrimedStateVar (varName v)

trConst :: Type t -> t -> K.Term
trConst Integer v     = K.ValueLiteral (show v)
trConst Real    v     = K.ValueLiteral (show v)
trConst Bool    True  = K.ValueLiteral "true"
trConst Bool    False = K.ValueLiteral "false"

initLocals :: Node -> [K.Term]
initLocals node =
  concatMap f (Map.toList $ nodeLocalVars node)
  where
    f (v, VarDescr t def) =
      case def of
        Pre     c _ -> [mkEquality (trVar v) (trConst t c)]
        Expr    e   -> [mkEquality (trVar v) (trExpr False e)]
        Constrs cs  -> map (trExpr False) cs

transLocals :: Node -> [K.Term]
transLocals node =
  concatMap f (Map.toList $ nodeLocalVars node)
  where
   f (v, VarDescr _ def) =
      case def of
        Pre _ v' -> [mkEquality (trPrimedVar v) (trVar v')]
        Expr e   -> [mkEquality (trPrimedVar v) (trExpr True e)]
        Constrs cs  -> map (trExpr True) cs

trExpr :: Bool -> Expr t -> K.Term
trExpr primed = tr
  where
    tr :: forall t . Expr t -> K.Term
    tr (Const t c) = trConst t c
    tr (Ite _ c e1 e2) = K.FunApp "ite" [tr c, tr e1, tr e2]
    tr (Op1 _ op e) = K.FunApp (show op) [tr e]
    tr (Op2 _ op e1 e2) = K.FunApp (show op) [tr e1, tr e2]
    tr (VarE _ v) = if primed then trPrimedVar v else trVar v
