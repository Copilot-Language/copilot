{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module Copilot.Compile.C.CodeGen
  ( codegen
  , gather
  , vars
  , funcs
  ) where

import Copilot.Core as CP

import Copilot.Compile.C.Tmp
import Copilot.Compile.C.Util
import Copilot.Compile.C.Translation

import Language.C99.AST as C  ( BlockItem     (..)
                              , Decln
                              , FunDef
                              , Expr          (..)
                              , Init          (..)
                              , FunDef        (..)
                              , Declr         (..)
                              , DirectDeclr   (..)
                              , CompoundStmt  (..)
                              , Stmt          (..)
                              , Ptr           (..)
                              , JumpStmt  (JSReturn)
                              )
import Language.C99.Util  ( static
                          , ident
                          , bool
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

data Generator = Generator
  { genBuff   :: String
  , genVal    :: String
  , genFunc   :: String
  , genStream :: Stream
  }

data Guard = Guard
  { guardName     :: String
  , guardTrigger  :: Trigger
  }

{- Abstract program, used to gather information -}
data AProgram = AProgram
  { streams     :: [Stream]
  , generators  :: [Generator]
  , trigguards  :: [Guard]
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
gather spec = AProgram  { streams     = streams
                        , generators  = map genname streams
                        , trigguards  = map guardname triggers
                        , trigargs    = concatMap argnames triggers
                        , externals   = M.toList externals'
                        } where
  streams = specStreams spec
  triggers = specTriggers spec

  genname :: Stream -> Generator
  genname s = Generator { genVal  = basename
                        , genBuff = basename ++ "_buff"
                        , genFunc = basename ++ "_gen"
                        , genStream = s
                        } where
    basename = "s" ++ show (streamId s)

  guardname :: Trigger -> Guard
  guardname t = Guard { guardName    = triggerName t ++ "_guard"
                      , guardTrigger = t
                      }

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


{- Translate abstract program to a concrete one -}
reify :: AProgram -> Program
reify ap = Program  { funcs = concat $  [ map (streamgen ss) gens
                                        , map (guardgen ss) guards
                                        , map (uncurry (arggen ss)) args
                                        ]
                    , vars = []
                    } where
  ss = streams ap
  gens   = generators ap
  guards = trigguards ap
  args   = trigargs ap
  exts   = externals ap

{- Create a function that generates the stream -}
streamgen :: [Stream] -> Generator -> FunDef
streamgen ss (Generator _ _ func (Stream _ _ expr ty)) = fd where
  fd = FD (static $ cty) dr Nothing body
  cty = ty2type ty
  dr = case ty of
    Array _ -> Dr (Just $ PBase Nothing) (DDIdent $ ident func)
    _       -> Dr Nothing (DDIdent $ ident func)
  body = fungen ss expr

{- Write function for the guard of a trigger -}
guardgen :: [Stream] -> Guard -> FunDef
guardgen ss (Guard funname (Trigger _ guard _)) = fd where
  fd = fundef funname (static $ bool) [] body
  body = fungen ss guard

{- Write function that generates stream for argument of a trigger -}
arggen :: [Stream] -> String -> UExpr -> FunDef
arggen ss funname (UExpr t expr) = FD (static $ cty) dr Nothing body where
  cty = ty2type t
  dr = case t of
    Array _ -> Dr (Just $ PBase Nothing) (DDIdent $ ident funname)
    _       -> Dr Nothing (DDIdent $ ident funname)
  body = fungen ss expr

{- Generic function that writes the bodies of all generator / guard / args -}
fungen :: [Stream] -> CP.Expr a -> CompoundStmt
fungen ss expr = body where
  (e, env) = runState (cexpr expr) emptyFunState
  drops = combine (ids env) ss
  (bs, vars) = runState (mapM streambuff drops) []
  body = CS $ concat  [ map BIDecln vars
                      , concat bs
                      , stmts env
                      , [ BIStmt $ SJump $ JSReturn $ Just e ]
                      ]

{- Code reading current value of a (dropped) stream -}
streambuff :: (Stream, Word32) -> State [Decln] [BlockItem]
streambuff (Stream i buff _ ty, drop) = do
  let cty = ty2type ty
      basename = "s" ++ show i

      loc = basename ++ "_loc"
      ptr = basename ++ "_ptr"
      buffname = basename ++ "_buff"
      idx = "idx"
      dropped = "dropped"
      buffsize = length buff

      vars = case ty of
        Array _ -> [ vardef cty [ptrdeclr loc Nothing] ]
        _       -> [ vardef cty [declr    loc Nothing] ]

      body =  [ BIStmt $ SCompound $ CS $ idxcode ++ [
                  BIStmt $ assign (EIdent $ ident loc) (EIndex (EIdent $ ident buffname) (var idx))
                ]
              ]

      idxcode = case drop of
        0 -> [ BIDecln $ vardef size_t [declr idx (Just $ IExpr $ var ptr)] ]
        _ -> [ BIDecln $ vardef size_t [declr dropped (Just $ IExpr $ EAdd (var ptr) (constint drop))]
             , BIDecln $ vardef size_t [declr idx (Just $ IExpr $ EMod (var dropped) (constint buffsize))]
             ]

  vars' <- get
  put (vars ++ vars')
  return body

{- Translate Spec to Program, used by the compile function -}
codegen :: Spec -> Program
codegen = reify.gather
