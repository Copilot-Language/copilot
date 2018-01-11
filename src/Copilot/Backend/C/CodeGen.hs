{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module Copilot.Backend.C.CodeGen
  ( compile
  ) where

import Copilot.Core as CP hiding (SExpr)
import Copilot.Core.PrettyPrint

import Copilot.Backend.C.Normalize
import Copilot.Backend.C.Tmp

import Language.C99.AST as C
import Language.C99.Util
import Language.C99.Pretty

import Control.Monad.State ( State
                           , put
                           , get
                           , runState
                           , execState
                           , evalState
                           )
import Text.PrettyPrint (render)
import Data.List  (union)


{- Attribute Grammar like state -}
data FunEnv = FunEnv
  { stmts   :: [BlockItem]
  , ids     :: [(Id, Word32)]
  }

type FunState = State FunEnv
emptyFunState = FunEnv { stmts = []
                       , ids   = []
                       }

data ProgEnv = ProgEnv
  { vars  :: [Decln]
  , funcs :: [FunDef]
  }
type ProgState = State ProgEnv
emptyProgState = ProgEnv { vars   = []
                         , funcs  = []
                         }

putglobvar :: Decln -> State ProgEnv ()
putglobvar s = do env <- get
                  put $ env { vars = vars env ++ [s] }

putfunc :: FunDef -> State ProgEnv ()
putfunc f = do  env <- get
                put $ env { funcs = funcs env ++ [f] }


ty2type :: Type a -> DeclnSpecs
ty2type ty = case ty of
  Int8    -> typedefty "__int8_t"
  Int16   -> typedefty "__int16_t"
  Int32   -> typedefty "__int32_t"
  Int64   -> typedefty "__int64_t"
  Word8   -> typedefty "__uint8_t"
  Word16  -> typedefty "__uint16_t"
  Word32  -> typedefty "__uint32_t"
  Word64  -> typedefty "__uint64_t"
  Float   -> float
  Double  -> double
  Bool    -> typedefty "bool"

op1 :: Op1 a b -> C.Expr -> C.Expr
op1 op e = case op of
  Not     -> EUn UNot e
  Abs _   -> funcall "abs" [e]
  Sign _  -> funcall "sign" [e] -- TODO implement function
  Recip _ -> EDiv (constint 1) (var "e")
  Exp _   -> funcall "exp" [e]
  Sqrt _  -> funcall "sqrt" [e]
  Log _   -> funcall "log" [e]
  Sin _   -> funcall "sin" [e]
  Cos _   -> funcall "cos" [e]
  Asin _  -> funcall "asin" [e]
  Atan _  -> funcall "atan" [e]
  Acos _  -> funcall "acos" [e]
  Sinh _  -> funcall "sinh" [e]
  Tanh _  -> funcall "tanh" [e]
  Cosh _  -> funcall "cosh" [e]
  Asinh _ -> funcall "asinh" [e]
  Atanh _ -> funcall "atanh" [e]
  Acosh _ -> funcall "acosh" [e]
  BwNot _ -> EUn UBNot e
  Cast ty _ -> undefined -- TODO

op2 :: Op2 a b c -> C.Expr -> C.Expr -> C.Expr
op2 op = case op of
  And     -> ELAnd
  Or      -> ELOr
  Add _  -> EAdd
  Sub _  -> ESub
  Mul _  -> EMult
  Mod _  -> EMod
  Div _  -> EDiv
  Fdiv _ -> EDiv
  Pow _  -> \b n -> funcall "pow" [b, n]
  Logb _ -> undefined -- TODO
  Eq _   -> EEq
  Ne _   -> ENEq
  Le _   -> ELE
  Ge  _  -> EGE
  Lt   _ -> ELT
  Gt _   -> EGT
  BwAnd _   -> EAnd
  BwOr _    -> EOr
  BwXor _   -> EXor
  BwShiftL _ _ -> EShiftL
  BwShiftR _ _ -> EShiftR

op3 :: Op3 a b c d -> C.Expr -> C.Expr -> C.Expr -> C.Expr
op3 op = case op of
  Mux _   -> ECond


constty :: Type a -> a -> C.Expr
constty Int8    = constint
constty Int16   = constint
constty Int32   = constint
constty Int64   = constint
constty Word8   = constword
constty Word16  = constword
constty Word32  = constword
constty Word64  = constword
constty Bool    = constbool
constty Float   = constfloat
constty Double  = constdouble




{- Translate to a C-expression -}
cexpr :: CP.Expr a -> FunState C.Expr
cexpr (Const ty x) = return $ constty ty x

cexpr (Local ty1 ty2 n e1 e2) = do
  let cty = ty2type ty1
  e1' <- cexpr e1
  env <- get
  let decln = BIDecln $ vardef cty [declr' (ident n) e1']
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

cexpr (ExternVar ty n args) = do return $ var n

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


{- Write function to generate a stream -}
streamgen :: [Stream] -> Stream -> FunDef
streamgen ss (Stream id buff expr ty) = fundef name (static $ cty) [] body  where
  base = "s" ++ show id
  name = base ++ "_gen"
  cty = ty2type ty

  (e, env) = runState (cexpr expr) emptyFunState
  s (i,n) = (findstream i ss,n)
  body = CS $ concatMap (streambuff.s) (ids env) ++
    [BIStmt $ SJump $ JSReturn $ Just e]


{- Write code that reads from buffer -}
streambuff :: (Stream, Word32) -> [BlockItem]
streambuff ((Stream id buff _ ty),drop) = body where
  base = "s" ++ show id
  val = base ++ "_val"
  cty = ty2type ty

  dropped = "dropped"
  ptr = base ++ "_ptr"
  buffsize = length buff
  idx = "idx"
  buffname = base ++ "_buff"

  body =  [ BIDecln $ vardef cty [declr (ident val)]
          , BIStmt $ SCompound $ CS
            [ BIDecln $ vardef size_t [declr' (ident dropped) (EAdd (var ptr) (constint drop))]
            , BIDecln $ vardef size_t [declr' (ident idx) (EMod (var dropped) (constint buffsize))]
            , BIStmt $ assign (EIdent $ ident val) (EIndex (EIdent $ ident buffname) (var idx))
            ]
          ]


{- Write function for guard of trigger -}
guardgen :: [Stream] -> Trigger -> FunDef
guardgen ss (Trigger name guard args) = fundef funname (static $ bool) [] body where
  funname = name ++ "_guard"

  (e, env) = runState (cexpr guard) emptyFunState
  s (i,n) = (findstream i ss,n)
  body = CS $ concatMap (streambuff.s) (ids env) ++
    [BIStmt $ SJump $ JSReturn $ Just e]


{- Write arg functions for trigger -}
argsgen :: [Stream] -> Trigger -> [FunDef]
argsgen ss (Trigger name _ args) = map (uncurry $ arggen) (zip args [0..]) where
  arggen (UExpr ty uexpr) n = fundef funname (static $ cty) [] body where
    cty = ty2type ty
    funname = name ++ "_arg" ++ show n

    (uexpr', env) = runState (cexpr uexpr) emptyFunState
    s (i,n) = (findstream i ss,n)
    body = CS $ concatMap (streambuff.s) (ids env) ++
      [BIStmt $ SJump $ JSReturn $ Just uexpr']


{- Write step() function -}
step :: [Stream] -> [Trigger] -> FunDef
step ss ts = fundef "step" (static $ void) [] body where
  body = CS $ conds ++ assigns ++ buffers ++ ptrs

  {- Update stream values -}
  assigns = map genassign ss
  genassign (Stream id _ _ _) = stmt where
    name = "s" ++ show id
    stmt = BIStmt $ assign (EIdent $ ident name) (funcall (name ++ "_gen") [])

  {- Update buffers -}
  buffers = map updatebuff ss
  updatebuff (Stream id _ _ _) = stmt where
    base = "s" ++ show id
    --var = "s" ++ show id
    ptr = base ++ "_ptr"
    buffname = base ++ "_buff"
    idxbuff = EIndex (EIdent $ ident buffname) (EIdent $ ident ptr)
    stmt = BIStmt $ assign idxbuff (var base)

  {- Check triggers -}
  conds = map triggercond ts
  triggercond (Trigger n _ args) = BIStmt $ SSelect $ SSIf cond call where
    n' = n ++ "_guard"
    cond = funcall n' []
    args' = map argcall (take (length args) [0..])
    argcall i = funcall (n ++ "_arg" ++ (show i)) []
    call = SExpr $ ES $ Just $ funcall n args'

  {- Update pointers -}
  ptrs = concatMap ptrinc ss
  ptrinc (Stream id buff _ _) = [ BIStmt $ SExpr $ ES $ Just inc
                                , BIStmt mod
                                ] where
    name = "s" ++ show id ++ "_ptr"
    buffsize = length buff
    inc = EInc (var name)
    mod = assign (EIdent $ ident name) (EMod (var name) (constint buffsize))


{- Take a spec and generate all declarations and functions -}
codegen :: Spec -> State ProgEnv ()
codegen s = do
  let streams = specStreams s
      triggers = specTriggers s
      -- TODO
      --observers = specObservers s
      --props = specProperties s

      sf = map (streamgen streams) streams
      tf = map (guardgen streams) triggers
      af = map (argsgen streams) triggers

  {- Write buffer and pointer declrations -}
  mapM putfunc sf
  mapM putfunc tf
  mapM putfunc (concat af)
  putfunc (step streams triggers)
  mapM_ streamvars streams


streamvars :: Stream -> State ProgEnv ()
streamvars (Stream id buff _ ty) = do
  let name  = "s" ++ show id ++ "_buff"
      ptr   = "s" ++ show id ++ "_ptr"
      s     = "s" ++ show id
      cty   = ty2type ty
      buff' = map (constty ty) buff
  putglobvar $ vardef cty    [declr' (ident s) (constty ty (head buff))]
  putglobvar $ vardef cty    [ddarray name buff']
  putglobvar $ vardef size_t [declr' (ident ptr) (constint 0)]


{- A function to test compilation -}
testcompile :: Spec -> IO ()
testcompile s = do
  let s' = normalize s
  putStrLn $ prettyPrint s
  putStrLn dots
  compile s
  putStrLn line
  putStrLn $ prettyPrint s'
  putStrLn dots
  compile s'
  where
    dots = ". . . . . . . . . ."
    line = "-------------------"

{- Compile function, currently prints to stdout -}
compile :: Spec -> IO ()
compile s = do
    putStrLn $ unlines headers
    putStrLn $ render $ pretty (compile' s)
    where
      headers = [ "#include <stdio.h>"
                , "#include <stdbool.h>"
                ]

      compile' :: Spec -> TransUnit
      compile' s = TransUnit (vars' ++ funcs') where
        vars' = map EDDecln (vars defs)
        funcs' = map EDFunDef (funcs defs)

        defs = execState (codegen s) emptyProgState
