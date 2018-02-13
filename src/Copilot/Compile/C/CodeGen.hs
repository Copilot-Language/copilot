{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module Copilot.Compile.C.CodeGen where

import Copilot.Core as CP

import Copilot.Compile.C.Tmp
import Copilot.Compile.C.Util
import Copilot.Compile.C.Translation

import Language.C99.AST as C  ( BlockItem (..)
                              , Decln
                              , FunDef
                              , Expr      (..)
                              , Init      (..)
                              )

import Control.Monad.State ( State
                           , put
                           , get
                           , runState
                           )
import Data.List (union)
import Data.Map (Map)
import qualified Data.Map as M


{- Attribute Grammar like state -}
data FunEnv = FunEnv
  { stmts   :: [BlockItem]
  , ids     :: [(Id, Word32)]
  }

type FunState = State FunEnv
emptyFunState :: FunEnv
emptyFunState = FunEnv { stmts = []
                       , ids   = []
                       }

{- Abstract program, used to gather information -}
data AProgram = AProgram
  { generators  :: [(String, Stream)]
  , trigguards  :: [(String, Trigger)]
  , trigargs    :: [(String, UExpr)]
  , externals   :: [(String, UType)]
  }

{- Concrete program, used to list global variables and functions -}
data Program = Program
  { vars  :: [Decln]
  , funcs :: [FunDef]
  }


{- Translate Copilot expression to a C-expression -}
cexpr :: CP.Expr a -> FunState C.Expr
cexpr (Const ty x) = return $ constty ty x

cexpr (Local ty1 ty2 n e1 e2) = do
  let cty = ty2type ty1
  e1' <- cexpr e1
  env <- get
  let decln = BIDecln $ vardef cty [declr n (Just $ IExpr e1')]
  put $ env { stmts = (stmts env) ++ [decln] }
  e2' <- cexpr e2
  return e2'

cexpr (Var ty n)   = return $ var n

cexpr (Drop ty n id) = do
  let basename = "s" ++ show id
      val = basename ++ "_val"
  env <- get
  put $ env { ids = (ids env) `union` [(id, n)] }
  return $ var val

cexpr (ExternVar ty n args) = return $ var n

cexpr (Op1 op e) = do
  e' <- cexpr e
  return $ op1 op e'

cexpr (Op2 op e1 e2) = do
  e1' <- cexpr e1
  e2' <- cexpr e2
  return $ op2 op e1' e2'

cexpr (Op3 op e1 e2 e3) = do
  e1' <- cexpr e1
  e2' <- cexpr e2
  e3' <- cexpr e3
  return $ op3 op e1' e2' e3'


{- Gather required information from Spec, and create names for functions -}
gather :: Spec -> AProgram
gather spec = AProgram  { generators  = map genname streams
                        , trigguards  = map guardname triggers
                        , trigargs    = concatMap argnames triggers
                        , externals   = M.toList externals'
                        } where
  streams = specStreams spec
  triggers = specTriggers spec

  genname :: Stream -> (String, Stream)
  genname s   = ("s" ++ show (streamId s) ++ "_gen", s)

  guardname :: Trigger -> (String, Trigger)
  guardname t = (triggerName t ++ "_guard", t)

  argnames :: Trigger -> [(String, UExpr)]
  argnames (Trigger name guard args) = args' where
    args' = map argname (zip [0..] args)
    argname (n, a) = (name ++ "_arg" ++ show n, a)

  externals' :: Map String UType
  externals' = M.unions $ (map exstreams streams) ++ (map extriggers triggers)
  exstreams (Stream _ _ e _) = externs e
  extriggers (Trigger _ guard args) = M.unions (externs guard : map exuexprs args)
  exuexprs (UExpr _ e) = externs e

  externs :: CP.Expr a -> Map String UType
  externs e = case e of
    Local _ _ _ e1 e2   -> (externs e1) `M.union` (externs e2)
    ExternVar t name _  -> M.singleton name (UType t)
    Op1 _ e             -> externs e
    Op2 _ e1 e2         -> externs e1 `M.union` externs e2
    Op3 _ e1 e2 e3      -> externs e1 `M.union` externs e2 `M.union` externs e3
    _                   -> M.empty

{- Translate Spec to Program, used by the compile function -}
codegen :: Spec -> Program
codegen = undefined
