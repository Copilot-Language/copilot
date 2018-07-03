{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module Copilot.Compile.C.CodeGen
  ( codegen
  , gather
  , vars
  , funcs
  ) where

import Copilot.Core as CP hiding (index)

import Copilot.Compile.C.Tmp
import Copilot.Compile.C.Util
import Copilot.Compile.C.Translation
import Copilot.Compile.C.Meta

import Copilot.Compile.ACSL.CodeGen
import Text.PrettyPrint

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
                              , ExprStmt (..)
                              , SelectStmt  (SSIf)
                              , InitDeclr (..)
                              )
import Language.C99.Util  ( static
                          , ident
                          , bool
                          , void
                          )

import Control.Monad.State ( State
                           , put
                           , get
                           , runState
                           )
import Data.List (union, unionBy)


{- Attribute Grammar like state -}
data FunEnv = FunEnv
  { stmts   :: [BlockItem]
  , ids     :: [(Id, Word32, String)]
  }

type FunState = State FunEnv
emptyFunState :: FunEnv
emptyFunState = FunEnv { stmts = []
                       , ids   = []
                       }

{- Concrete program, used to list global variables and functions -}
data Program = Program
  { vars  :: [Decln]
  , funcs :: [(Doc, FunDef)]
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
  locname <- fresh id n
  return $ var locname where
    {- Generate a fresh name, if it is already in use.
       To avoid conflicts as much as possible, we append "_dropN" if n > 0. -}
    fresh :: Id -> Word32 -> FunState String
    fresh id drop = do
      env <- get
      let usednames = map third (ids env)
          dropped = dropname id n
          dropped_fresh x = dropped ++ "_" ++ (show x)
          locname = head $ dropWhile (flip elem usednames) freshnames
          freshnames = dropped:(dropped_fresh <$> [0..])
      put $ env { ids = (ids env) `union` [(id, drop, locname)] }
      return locname
    third (a,b,c) = c

cexpr (ExternVar ty n args) = case ty of
  Array _ -> return $ var (excpy n) -- Rename external arrays because of copying
  _       -> return $ var n

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
                        , externals   = externals'
                        } where
  streams = specStreams spec
  triggers = specTriggers spec

  genname :: Stream -> Generator
  genname s = Generator { genValName    = basename
                        , genBuffName   = basename ++ "_buff"
                        , genIndexName  = idxvar basename
                        , genFuncName   = basename ++ "_gen"
                        , genStream     = s
                        } where
    basename = basevar (streamId s)

  guardname :: Trigger -> Guard
  guardname t = Guard { guardName    = triggerName t ++ "_guard"
                      , guardCFunc   = triggerName t
                      , guardArgs    = argnames t
                      , guardTrigger = t
                      }

  argnames :: Trigger -> [Argument]
  argnames (Trigger name guard args) = args' where
    args' = map argname (zip [0..] args)
    argname (n, a) = Argument { argName = name ++ "_arg" ++ show n
                              , argExpr = a
                              }

  externals' :: [External]
  externals' = unions' $ (map exstreams streams) ++ (map extriggers triggers)
  exstreams (Stream _ _ e _) = externs e
  extriggers (Trigger _ guard args) = unions' (externs guard : map exuexprs args)
  exuexprs (UExpr _ e) = externs e

  externs :: CP.Expr a -> [External]
  externs e = case e of
    Local _ _ _ e1 e2   -> externs e1 `union'` externs e2
    ExternVar t name _  -> [ External { exName    = name
                                      , exLocName = excpy name
                                      , exType    = UType t
                                      } ]
    Op1 _ e             -> externs e
    Op2 _ e1 e2         -> externs e1 `union'` externs e2
    Op3 _ e1 e2 e3      -> externs e1 `union'` externs e2 `union'` externs e3
    _                   -> []

  union' = unionBy (\a b -> exName a == exName b)
  unions' = foldr union' []

{- Translate abstract program to a concrete one -}
reify :: AProgram -> Program
reify ap = Program  { funcs = concat $  [ map (streamgen ap) gens
                                        , map (guardgen ap) guards
                                        , map (arggen ap) args

                                        , [ step ap ]
                                        ]
                    , vars = globvars gens ++ exarrays exts
                    } where
  ss = streams ap
  gens   = generators ap
  guards = trigguards ap
  args   = concatMap guardArgs guards
  exts   = externals ap

{- Global variables -}
globvars :: [Generator] -> [Decln]
globvars gens = buffs ++ vals ++ idxs where
  (buffs, vals, idxs) = unzip3 $ map globvar gens

  globvar :: Generator -> (Decln, Decln, Decln)
  globvar (Generator buffname val idx _ (Stream _ buff _ ty)) =
    let cty = ty2type ty
        len = length buff
    in case ty of
      Array _ -> let len' = [len, size $ dim (head buff)] in
        ( vardef (static $ cty)     [arrdeclr buffname len' (Just $ initvals ty buff)]
        , vardef (static $ cty)     [ptrdeclr val (Just $ IExpr $ index buffname (constint 0))]
        , vardef (static $ size_t)  [declr idx (Just $ IExpr $ constint 0)]
        )

      _ ->
        ( vardef (static $ cty)     [arrdeclr buffname [len] (Just $ initvals ty buff)]
        , vardef (static $ cty)     [declr val (Just $ initval ty (head buff))]
        , vardef (static $ size_t)  [declr idx (Just $ IExpr $ constint 0)]
        )

{- Copies of external arrays -}
exarrays :: [External] -> [Decln]
exarrays exts = concatMap exarray exts where
  exarray :: External -> [Decln]
  exarray (External name locname (UType ty)) =
    case ty of
      Array _ -> let cty = ty2type ty
                     len = length $ fromIndex $ tyIndex ty
                 in [ vardef (static $ cty) [arrdeclr locname [len] Nothing] ]
      _       -> []

{- Create a function that generates the stream -}
streamgen :: AProgram -> Generator -> (Doc, FunDef)
streamgen ap g@(Generator _ _ _ func (Stream _ _ expr ty)) = (acsl, fd) where
  ss = streams ap
  fd = FD (static $ cty) dr Nothing body
  cty = ty2type ty
  dr = case ty of
    Array _ -> Dr (Just $ PBase Nothing) (DDIdent $ ident func)
    _       -> Dr Nothing (DDIdent $ ident func)
  body = fungen ss expr
  acsl = streamgenACSL ap g

{- Write function for the guard of a trigger -}
guardgen :: AProgram -> Guard -> (Doc, FunDef)
guardgen ap g@(Guard guardfunc _ _ (Trigger _ guard _)) = (acsl, fd) where
  ss = streams ap
  fd = fundef guardfunc (static $ bool) [] body
  body = fungen ss guard
  acsl = guardgenACSL ap g

{- Write function that generates stream for argument of a trigger -}
arggen :: AProgram -> Argument -> (Doc, FunDef)
arggen ap a@(Argument funname (UExpr ty expr)) = (acsl, fd) where
  ss = streams ap
  fd = FD (static $ cty) dr Nothing body
  cty = ty2type ty
  dr = case ty of
    Array _ -> Dr (Just $ PBase Nothing) (DDIdent $ ident funname)
    _       -> Dr Nothing (DDIdent $ ident funname)
  body = fungen ss expr
  acsl = arggenACSL ap a


type VarName = String
type Env = State [Id]

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
streambuff :: (Stream, Word32, String) -> State [Decln] [BlockItem]
streambuff (Stream i buff _ ty, drop, loc) = do
  let cty = ty2type ty
      basename = basevar i
      ptr = idxvar basename
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



{- The step function updates the current state -}
step :: AProgram -> (Doc, FunDef)
step ap = (acsl, fundef "step" void [] body) where
  body = CS $ concat  [ copyexts      exts
                      , triggers      guards
                      , update        gens
                      , updatearrays  gens
                      , updatebuffers gens
                      , updateindices gens
                      ] where
  gens = generators ap
  guards = trigguards ap
  exts = externals ap
  acsl = stepACSL ap

  {- Copy external arrays to make monitor reentrant -}
  copyexts :: [External] -> [BlockItem]
  copyexts exts = concatMap copyext exts where
    copyext :: External -> [BlockItem]
    copyext (External name locname (UType ty)) = case ty of
      Array _ -> let l = size $ tyIndex ty
                 in [ BIStmt $ SExpr $ ES $ Just $ funcall "memcpy" [ var locname
                                                                    , var name
                                                                    , ESizeof (var locname)
                                                                    ]
                    ]
      _       -> []

  {- Check guards and fire triggers accordingly -}
  triggers :: [Guard] -> [BlockItem]
  triggers gs = map triggercond gs where
    triggercond (Guard guardname cfunc args _) = BIStmt $ SSelect $ SSIf cond call where
      cond = funcall guardname []
      call = SExpr $ ES $ Just $ funcall cfunc args'
      args' = map (\a -> funcall (argName a) []) args

  {- Update stream values -}
  update :: [Generator] -> [BlockItem]
  update gens = map update' gens where
    update' gen = BIStmt $ assign (var (genValName gen)) (funcall (genFuncName gen) [])

  {- Copy current value of array to tmp value -}
  updatearrays :: [Generator] -> [BlockItem]
  updatearrays gens = concatMap updatearray gens where
    updatearray :: Generator -> [BlockItem]
    updatearray (Generator buff val idx _ (Stream _ b _ ty)) = case ty of
      Array tya ->
        [ BIDecln $ vardef (ty2type tya) [
            IDDeclr $ Dr Nothing (DDArray1 (DDIdent $ ident tmp) Nothing (Just $ constint l))
          ]
        , BIStmt $ SExpr $ ES $ Just $ funcall "memcpy" [var tmp, var val, ESizeof (var tmp)]
        ] where
          size = ESizeof (index buff (constint 0))
          tmp = val ++ "_tmp"
          idxbuff = index buff (var idx)
          l = length $ fromIndex $ tyIndex ty
      _ -> []

  {- Update buffers -}
  updatebuffers :: [Generator] -> [BlockItem]
  updatebuffers gens = map updatebuffer gens where
    updatebuffer (Generator buff val idx _ (Stream _ b _ ty)) = stmt where
      idxbuff = index buff (var idx)
      tmp = val ++ "_tmp"
      size = ESizeof (var tmp)
      stmt = case ty of
        Array _ -> BIStmt $ SExpr $ ES $ Just $ funcall "memcpy" [idxbuff, var tmp, size]
        _       -> BIStmt $ assign idxbuff (var val)

  {- Update indices / pointers in the arrays -}
  updateindices :: [Generator] -> [BlockItem]
  updateindices gens = incs ++ mods where
    (incs, mods) = unzip $ map updateindex gens
    updateindex (Generator _ _ idx _ (Stream _ buff _ _)) =
      ( BIStmt $ SExpr $ ES $ Just (EInc $ var idx)
      , BIStmt $ assign (var idx) (EMod (var idx) (constint buffsize))
      ) where
        buffsize = length buff



{- Translate Spec to Program, used by the compile function -}
codegen :: Spec -> Program
codegen = reify.gather
