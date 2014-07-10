--------------------------------------------------------------------------------

module Copilot.Kind.Kind2.Translate 
  ( toKind2
  , Style (..)
  ) where

import Copilot.Kind.TransSys
import Copilot.Kind.Misc.Utils

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap

import qualified Copilot.Kind.Kind2.AST as K

import Data.List (sort)

--------------------------------------------------------------------------------

-- | The following properties MUST hold for the given transition system :
-- * Nodes are sorted by topological order
-- * Nodes are `completed`, which means the dependency graph is transitive
--   and each node imports all the local variables of its dependencies

--------------------------------------------------------------------------------

type DepGraph = Map NodeId [NodeId]

--------------------------------------------------------------------------------

data Style = Inlined | Modular

toKind2 :: Style -> Spec -> K.File
toKind2 style spec = trSpec (complete spec') predCallsGraph
  where predCallsGraph = specDependenciesGraph spec'
        spec' = case style of
          Inlined -> inline spec
          Modular -> removeCycles spec

trSpec :: Spec -> DepGraph -> K.File
trSpec spec predCallsGraph = K.File preds props 
  where preds = map (trNode spec predCallsGraph) (specNodes spec)
        props = map trProp $ Map.toList (specProps spec)
  
trProp :: (PropId, ExtVar) -> K.Prop
trProp (pId, var) = K.Prop pId (trVar . extVarLocalPart $ var)

trNode :: Spec -> DepGraph -> Node -> K.PredDef
trNode spec predCallsGraph node = 
  K.PredDef { K.predId, K.predStateVars, K.predInit, K.predTrans }
  where
    predId = nodeId node
    predStateVars = gatherPredStateVars spec node
    predInit  = mkConj $ initLocals  node 
                         ++ predCalls True  spec predCallsGraph node
    predTrans = mkConj $ transLocals node 
                         ++ predCalls False spec predCallsGraph node

--------------------------------------------------------------------------------

-- The ordering really matters here because the variables
-- have to be given in this order in a pred call
-- Our convention :
-- * First the local variables, sorted by alphabetical order
-- * Then the imported variables, by alphabetical order on
--   the father node then by alphabetical order on the variable name

gatherPredStateVars :: Spec -> Node -> [K.StateVarDef]
gatherPredStateVars spec node = locals ++ imported
  where
    nodesMap = Map.fromList [(nodeId n, n) | n <- specNodes spec]
    extVarType :: ExtVar -> K.Type
    extVarType (ExtVar n v) = 
      case (nodeLocalVars (nodesMap ! n)) ! v of
        LVarDescr Integer _ -> K.Int
        LVarDescr Bool    _ -> K.Bool
   
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
trConst Integer v     = K.ValueLitteral (show v)
trConst Bool    True  = K.ValueLitteral "true"
trConst Bool    False = K.ValueLitteral "false"

--------------------------------------------------------------------------------

initLocals :: Node -> [K.Term]
initLocals node = 
  map f (Map.toList $ nodeLocalVars node)
  where 
    f (v, LVarDescr t def) = 
      mkEquality (trVar v) $ case def of
        Pre v _ -> trConst t v
        Expr e  -> trExpr False e
  

transLocals :: Node -> [K.Term]
transLocals node = 
  map f (Map.toList $ nodeLocalVars node)
  where 
   f (v, LVarDescr _ def) = 
      mkEquality (trPrimedVar v) $ case def of
        Pre _ v -> trVar v
        Expr e  -> trExpr True e
        
       
predCalls :: Bool -> Spec -> DepGraph -> Node -> [K.Term]
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
          concat . map nodeLocals . sort
          . nodeDependencies $ nodesMap ! callee
          
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
