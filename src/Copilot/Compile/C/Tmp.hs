{-# LANGUAGE GADTs #-}

module Copilot.Compile.C.Tmp where

-- All temporary stuff, that needs some final location / implementation

import Copilot.Core hiding (SExpr)
import Language.C99.AST as C
import Language.C99.Util

import Data.Maybe (fromJust)
import Data.List  (find)

-- TODO: find better solution to fix this problem
deop :: Op1 a b -> (Type a,Type b)
deop op = let bi x = (x,x) in case op of
  Not     -> bi Bool
  Abs ty  -> bi ty
  Sign ty -> bi ty

  Recip ty -> bi ty

  Exp ty   -> bi ty
  Sqrt ty  -> bi ty
  Log ty   -> bi ty
  Sin ty   -> bi ty
  Tan ty   -> bi ty
  Cos ty   -> bi ty
  Asin ty  -> bi ty
  Atan ty  -> bi ty
  Acos ty  -> bi ty
  Sinh ty  -> bi ty
  Tanh ty  -> bi ty
  Cosh ty  -> bi ty
  Asinh ty -> bi ty
  Atanh ty -> bi ty
  Acosh ty -> bi ty

  BwNot ty -> bi ty

  Cast ty1 ty2  -> (ty1, ty2)

deop2 :: Op2 a b c -> (Type a, Type b, Type c)
deop2 op = let tri x = (x,x,x) in case op of
  And     -> tri Bool
  Or      -> tri Bool
  Add ty  -> tri ty
  Sub ty  -> tri ty
  Mul ty  -> tri ty

  Mod ty  -> tri ty
  Div ty  -> tri ty

  Fdiv ty -> tri ty

  Pow ty  -> tri ty
  Logb ty -> tri ty

  Eq ty   -> (ty, ty, Bool)
  Ne ty   ->(ty, ty, Bool)

  Le ty   -> (ty, ty, Bool)
  Ge ty   -> (ty, ty, Bool)
  Lt ty   -> (ty, ty, Bool)
  Gt ty   -> (ty, ty, Bool)

  BwAnd ty    -> tri ty
  BwOr ty     -> tri ty
  BwXor ty    -> tri ty
  BwShiftL ty1 ty2  -> (ty1, ty2, ty1)
  BwShiftR ty1 ty2  -> (ty1, ty2, ty1)

deop3 :: Op3 a b c d -> (Type a, Type b, Type c, Type d)
deop3 (Mux ty) = (Bool, ty, ty, ty)




opname1 :: Op1 a b -> String
opname1 op = case op of
  Not      -> "not"
  Abs _    -> "abs"
  Sign _   -> "signum"
  Recip _  -> "recip"
  Exp _    -> "exp"
  Sqrt _   -> "sqrt"
  Log _    -> "log"
  Sin _    -> "sin"
  Tan _    -> "tan"
  Cos _    -> "cos"
  Asin _   -> "asin"
  Atan _   -> "atan"
  Acos _   -> "acos"
  Sinh _   -> "sinh"
  Tanh _   -> "tanh"
  Cosh _   -> "cosh"
  Asinh _  -> "asinh"
  Atanh _  -> "atanh"
  Acosh _  -> "acosh"
  BwNot _  -> "bnot"
  Cast _ _ -> "(cast)"


opname2 :: Op2 a b c -> String
opname2 op = case op of
  And          ->  "and"
  Or           ->  "or"
  Add      _   ->  "plus"
  Sub      _   ->  "min"
  Mul      _   ->  "mult"
  Div      _   ->  "div"
  Mod      _   ->  "mod"
  Fdiv     _   ->  "fdiv"
  Pow      _   ->  "pow"
  Logb     _   ->  "logb"
  Eq       _   ->  "Eq"
  Ne       _   ->  "NEq"
  Le       _   ->  "LE"
  Ge       _   ->  "GE"
  Lt       _   ->  "LT"
  Gt       _   ->  "GT"
  BwAnd    _   ->  "BAnd"
  BwOr     _   ->  "BOR"
  BwXor    _   ->  "BXOR"
  BwShiftL _ _ ->  "LS"
  BwShiftR _ _ ->  "RS"


var n = EIdent $ ident n

funcall :: String -> [C.Expr] -> C.Expr
funcall n es = EFunCall (EIdent $ ident n) (args es) where
  args [] = Nothing
  args (x:[]) = Just $ AELBase x
  args (x:xs) = Just $ foldl (\xs x -> AELCons xs x) (AELBase x) xs

constint x = EConst $ cint (fromIntegral x)
constword x = EConst $ cuint (fromIntegral x)
constfloat x = EConst $ cfloat x
constdouble x = EConst $ cdouble x
constbool True = EConst $ CEnum (ident "true")
constbool False = EConst $ CEnum (ident "false")



size_t = typedefty "size_t"


fundef :: String -> DeclnSpecs -> [Decln] -> CompoundStmt -> FunDef
fundef n ds args body = FD ds dr (args' args) body where
  dr = Dr Nothing (DDIdent $ ident n)
  args' [] = Nothing
  args' [x] = Just $ DnLBase x
  args' (x:xs)  = Just $ foldl (\xs x -> DnLCons xs x) (DnLBase x) xs

funarg ty name = Dn ty (Just $ IDLBase $ IDDeclr $ (Dr Nothing (DDIdent $ ident name)))




-- TODO
findstream :: Id -> [Stream] -> Stream
findstream id ss = fromJust $ find eq ss where
  eq s@(Stream id' _ _ _) = id == id'

-- Assign a variable
-- TODO
assign var e = SExpr $ ES $ Just $ EAssign AAssign var e


-- TODO
ddarray :: DirectDeclr -> [C.Expr] -> InitDeclr
ddarray dd xs = IDInit varn (vals xs) where
  varn = Dr Nothing (DDArray1 dd Nothing Nothing)
  vals (x:[]) = IArray (InitLBase Nothing (IExpr x))
  vals (x:xs) = IArray (foldl val base xs) where
    base = InitLBase Nothing (IExpr x)
    val xs x = InitLCons xs Nothing (IExpr x)
