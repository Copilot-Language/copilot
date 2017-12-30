{-# LANGUAGE GADTs #-}

module Copilot.Backend.C.CodeGen
  ( compile
  ) where

import Copilot.Core as CP
import Copilot.Core.PrettyPrint

import Copilot.Backend.C.Normalize
import Copilot.Backend.C.Tmp
import Copilot.Backend.C.Examples

import Language.Copilot (reify)

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

-- Used to describe the environment within a block
data FunEnv = FunEnv
  { vars  :: [Decln]
  }
emptyFunEnv = FunEnv []

data ProgEnv = ProgEnv
  { streams     :: [Decln]
  , generators  :: [FunDef]
  , triggers    :: [Stmt]
  }
emptyProgEnv = ProgEnv [] [] []

putstream :: Decln -> State ProgEnv ()
putstream s = do  env <- get
                  put $ env { streams = streams env ++ [s] }

putgen :: FunDef -> State ProgEnv ()
putgen g = do env <- get
              put $ env { generators = generators env ++ [g] }


recip_def :: FunDef
recip_def = FD float name args body where
  name = Dr Nothing (DDIdent $ ident "recip")
  args = Just $ funarg float "e"
  body = CS [ BIStmt $ SJump $ JSReturn $ Just $ EDiv (constint 1) (var "e") ]

funarg ty name = DnLBase (Dn ty (Just $ IDLBase $ IDDeclr $ (Dr Nothing (DDIdent $ ident name))))

ty2type :: Type a -> DeclnSpecs
ty2type ty = case ty of
  Int8    -> typedefty "int8_t"
  Int16   -> typedefty "int16_t"
  Int32   -> typedefty "int32_t"
  Int64   -> typedefty "int64_t"
  Word8   -> typedefty "uint8_t"
  Word16  -> typedefty "uint16_t"
  Word32  -> typedefty "uint32_t"
  Word64  -> typedefty "uint64_t"
  Float   -> float
  Double  -> double
  Bool    -> typedefty "bool"

op1 :: Op1 a b -> C.Expr -> C.Expr
op1 op e = case op of
  Not     -> EUn UNot e
  Abs _   -> funcall "abs" [e]
  Sign _  -> funcall "sign" [e] -- TODO implement function
  Recip _ -> funcall "recip" [e]
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
  And     -> EAnd
  Or      -> EOr
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
  BwAnd _   -> ELAnd
  BwOr _    -> ELOr
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

cexpr :: CP.Expr a -> State FunEnv C.Expr
cexpr (Const ty x) = return $ constty ty x
cexpr (Local ty1 ty2 n e1 e2) = do
  e1' <- cexpr e1
  FunEnv vars <- get
  put $ FunEnv $ vars ++ [vardef (ty2type ty1) [declr' (ident n) e1']]
  e2' <- cexpr e2
  return e2'
cexpr (Var ty n)   = return $ var n
cexpr (Drop ty 0 id) = return $ var ("s"++show id)
cexpr (Drop ty n id) = return $ funcall ("s"++show id++"_gen") [EAdd (var "t") (constint n) ]
cexpr (ExternVar ty n args) = do return $ var n
cexpr (Op1 op e)   = do
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


{- Generate global variable and generator of stream -}
streamcode :: Stream -> State ProgEnv ()
streamcode (Stream id buff expr ty) = do
  let cty = ty2type ty
      name = "s" ++ show id
      genname = name ++ "_gen"
      globvar = vardef (static $ cty) [declr $ ident name]
      generator = fundef genname cty body
      body = CS $ (map BIDecln locvars) ++ [BIStmt switch]

      defcase = BIStmt $ SLabeled $ LSDefault $ SJump $ JSReturn $ Just expr'

      switch = SSelect $ SSSwitch (var "t") (SCompound $ CS (cases ++ [defcase]))
      cases = map f (zip [0..] buff) where
        f (t, e) = BIStmt $ SLabeled $ LSCase (constint t) (SJump $ JSReturn $ Just (constty ty e))

      (expr',FunEnv locvars) = runState (cexpr expr) emptyFunEnv
  putstream globvar
  putgen generator

--triggercode :: Trigger -> State ProgEnv ()
triggercode :: Trigger -> State FunEnv Stmt
triggercode (Trigger name guard args) = do
  let f (UExpr _ uexpr) = cexpr uexpr
  args' <- mapM f args
  guard' <- cexpr guard
  let trigcode = SSelect $ SSIf guard' body
      body = C.SExpr $ ES $ Just $ funcall name args'
  return $ trigcode

{- Take a spec and generate all declarations and functions -}
codegen :: Spec -> State ProgEnv ()
codegen s = do
  let streams = specStreams s
      triggers = specTriggers s
      observers = specObservers s
      props = specProperties s
  mapM_ streamcode streams
  putgen (step_def triggers)

step_def :: [Trigger] -> FunDef
step_def ts = evalState (step_def' ts) emptyFunEnv where
  step_def' :: [Trigger] -> State FunEnv FunDef
  step_def' ts = do
    stmts <- mapM triggercode ts
    FunEnv vars <- get
    let body = CS $ (map BIDecln vars) ++ (map BIStmt stmts)
    return $ fundef "step" void body


{- Currently a function to test compilation -}
compile :: Spec -> IO ()
compile s = do
  let s' = normalize s
  putStrLn $ prettyPrint s
  putStrLn dots
  putStrLn $ render $ pretty (compile' s)
  putStrLn line
  --putStrLn $ prettyPrint s'
  --putStrLn dots
  putStrLn $ render $ pretty (compile' s')
  where
    compile' :: Spec -> TransUnit
    compile' s = TransUnit (vars ++ gens) where
      vars = map EDDecln (streams defs)
      gens = map EDFunDef (generators defs)

      defs = execState (codegen s) emptyProgEnv

    dots = ". . . . . . . . . ."
    line = "-------------------"
