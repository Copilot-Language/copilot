--------------------------------------------------------------------------------

{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE Safe           #-}
{-# LANGUAGE ViewPatterns   #-}

-- | Convert modular transition systems ('TransSys') into Kind2 file
-- specifications.
module Copilot.Theorem.Kind2.Translate
  ( toKind2
  , Style (..)
  ) where

import Copilot.Theorem.TransSys
import qualified Copilot.Theorem.Kind2.AST as K

import Control.Exception.Base (assert)

import Data.Function (on)
import Data.Maybe (fromJust)

import Data.List (sort, sortBy)
import Data.Map (Map, (!))

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap

--------------------------------------------------------------------------------

-- The following properties MUST hold for the given transition system :
-- * Nodes are sorted by topological order
-- * Nodes are `completed`, which means the dependency graph is transitive
--   and each node imports all the local variables of its dependencies
--

--------------------------------------------------------------------------------

type DepGraph = Map NodeId [NodeId]

--------------------------------------------------------------------------------

-- | Style of the Kind2 files produced: modular (with multiple separate nodes),
-- or all inlined (with only one node).
--
-- In the modular style, the graph is simplified to remove cycles by collapsing
-- all nodes participating in a strongly connected components.
--
-- In the inlined style, the structure of the modular transition system is
-- discarded and the graph is first turned into a /non-modular transition/
-- /system/ with only one node, which can be then converted into a Kind2 file.
data Style = Inlined | Modular

-- | Produce a Kind2 file that checks the properties specified.
toKind2 :: Style     -- ^ Style of the file (modular or inlined).
        -> [PropId]  -- ^ Assumptions
        -> [PropId]  -- ^ Properties to be checked
        -> TransSys  -- ^ Modular transition system holding the system spec
        -> K.File
toKind2 style assumptions checkedProps spec =
  addAssumptions spec assumptions
  $ trSpec (complete spec') predCallsGraph assumptions checkedProps
  where predCallsGraph = specDependenciesGraph spec'
        spec' = case style of
          Inlined -> inline spec
          Modular -> removeCycles spec

trSpec :: TransSys -> DepGraph -> [PropId] -> [PropId] -> K.File
trSpec spec predCallsGraph _assumptions checkedProps = K.File preds props
  where preds = map (trNode spec predCallsGraph) (specNodes spec)
        props = map trProp $
          filter ((`elem` checkedProps) . fst) $
          Map.toList (specProps spec)

trProp :: (PropId, ExtVar) -> K.Prop
trProp (pId, var) = K.Prop pId (trVar . extVarLocalPart $ var)

trNode :: TransSys -> DepGraph -> Node -> K.PredDef
trNode spec predCallsGraph node =
  K.PredDef { K.predId, K.predStateVars, K.predInit, K.predTrans }
  where
    predId = nodeId node
    predStateVars = gatherPredStateVars spec node
    predInit  = mkConj $ initLocals  node
                         ++ map (trExpr False) (nodeConstrs node)
                         ++ predCalls True spec predCallsGraph node
    predTrans = mkConj $ transLocals node
                         ++ map (trExpr True) (nodeConstrs node)
                         ++ predCalls False spec predCallsGraph node

addAssumptions :: TransSys -> [PropId] -> K.File -> K.File
addAssumptions spec assumptions (K.File {K.filePreds, K.fileProps}) =
  K.File (changeTail aux filePreds) fileProps
  where
    changeTail f (reverse -> l) = case l of
      []     -> error "impossible"
      x : xs -> reverse $ f x : xs

    aux pred =
      let init'  = mkConj ( K.predInit  pred : map K.StateVar vars )
          trans' = mkConj ( K.predTrans pred : map K.PrimedStateVar vars )
      in pred { K.predInit = init', K.predTrans = trans' }

    vars =
      let bindings   = nodeImportedVars (specTopNode spec)
          toExtVar a = fromJust $ Map.lookup a (specProps spec)
          toTopVar (ExtVar nId v) = assert (nId == specTopNodeId spec) v
      in map (varName . toTopVar . toExtVar) assumptions

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

predCalls :: Bool -> TransSys -> DepGraph -> Node -> [K.Term]
predCalls isInitCall spec predCallsGraph node =
  map mkCall toCall
  where
    nid = nodeId node
    toCall = predCallsGraph ! nid
    nodesMap = Map.fromList [(nodeId n, n) | n <- specNodes spec]

    nodeLocals n =
      map (ExtVar n) . sort . Map.keys
      . nodeLocalVars $ (nodesMap ! n)

    mkCall callee
      | isInitCall =
          K.PredApp callee K.Init (argsSeq trVar)
      | otherwise  =
          K.PredApp callee K.Trans (argsSeq trVar ++ argsSeq trPrimedVar)
      where

        calleeLocals = nodeLocals callee
        calleeImported =
          (concatMap nodeLocals . sort . nodeDependencies) $ nodesMap ! callee

        localAlias trVarF ev =
          case Bimap.lookupR ev $ nodeImportedVars node of
            Nothing -> error $
              "This spec is not complete : "
              ++ show ev ++ " should be imported in " ++ nid
            Just v -> trVarF v

        argsSeq trVarF =
          map (localAlias trVarF) (calleeLocals ++ calleeImported)

--------------------------------------------------------------------------------

trExpr :: Bool -> Expr t -> K.Term
trExpr primed = tr
  where
    tr :: forall t . Expr t -> K.Term
    tr (Const t c) = trConst t c
    tr (Ite _ c e1 e2) = K.FunApp "ite" [tr c, tr e1, tr e2]
    tr (Op1 _ op e) = K.FunApp (show op) [tr e]
    tr (Op2 _ op e1 e2) = K.FunApp (show op) [tr e1, tr e2]
    tr (VarE _ v) = if primed then trPrimedVar v else trVar v

--------------------------------------------------------------------------------
