--------------------------------------------------------------------------------

module Copilot.Kind.IL.Spec
  ( module Copilot.Kind.Misc.Type
  , module Copilot.Kind.Misc.Operators           
  , Type (..)
  , Op1  (..)
  , Op2  (..)
  , SeqId
  , SeqIndex (..)
  , SeqDescr (..)
  , FunName
  , UnintFunDescr (..)
  , Expr (..)
  , Spec (..)
  , Constraint
  , PropId
    
  , typeOf
  , _n_
  , _n_plus
  , iconst
  , evalAt
  ) where

import Copilot.Kind.Misc.Type
import Copilot.Kind.Misc.Operators

import Data.Map (Map)

--------------------------------------------------------------------------------

type SeqId    =  String

type Offset   =  Integer

data SeqIndex = Fixed Integer | Var Offset

data Expr t where
  Const  :: Type t -> t -> Expr t
  Ite    :: Type t -> Expr Bool -> Expr t -> Expr t -> Expr t
  Op1    :: Type t -> Op1 x t -> Expr x -> Expr t
  Op2    :: Type t -> Op2 x y t -> Expr x -> Expr y -> Expr t
  SVal   :: Type t -> SeqId -> SeqIndex -> Expr t
  FunApp :: Type t -> FunName -> [U Expr] -> Expr t

--------------------------------------------------------------------------------

type FunName = String
data UnintFunDescr = forall t . UnintFunDescr
  { funName      :: FunName
  , funRetType   :: Type t 
  , funArgsTypes :: [U Type] }

--------------------------------------------------------------------------------

type PropId = String

type Constraint = Expr Bool

data SeqDescr = forall t . SeqDescr 
  { seqId    :: SeqId
  , seqType  :: Type t }
  
data Spec = Spec 
  { modelInit   :: [Constraint]
  , modelRec    :: [Constraint]
  , properties  :: Map PropId Constraint
  , depth       :: Int
  , sequences   :: [SeqDescr]
  , unintFuns   :: [UnintFunDescr] }
                   
--------------------------------------------------------------------------------

typeOf :: Expr a -> Type a
typeOf e = case e of
  Const  t _       -> t
  Ite    t _ _ _   -> t
  Op1    t _ _     -> t
  Op2    t _ _ _   -> t
  SVal   t _ _     -> t
  FunApp t _ _     -> t
  
_n_ :: SeqIndex
_n_ = Var 0

_n_plus :: (Integral i) => i -> SeqIndex
_n_plus d = Var (toInteger d)

iconst :: (Integral i) => i -> Expr Integer
iconst n = Const Integer (toInteger n)

evalAt :: SeqIndex -> Expr t -> Expr t
evalAt _ (Const t e) = Const t e
evalAt i (Op1 t op e) = Op1 t op (evalAt i e)
evalAt i (Op2 t op e1 e2) = Op2 t op (evalAt i e1) (evalAt i e2)
evalAt i (Ite t c e1 e2) = Ite t (evalAt i c) (evalAt i e1) (evalAt i e2)
evalAt i (FunApp t name args) = FunApp t name $ map (\(U e) -> U $ evalAt i e) args

evalAt _ e@(SVal _ _ (Fixed _)) = e
evalAt (Fixed n) (SVal t s (Var d)) = SVal t s (Fixed $ n + d)
evalAt (Var   k) (SVal t s (Var d)) = SVal t s (Var   $ k + d)

--------------------------------------------------------------------------------
