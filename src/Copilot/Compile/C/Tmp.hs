{-# LANGUAGE GADTs #-}

module Copilot.Compile.C.Tmp where

-- All temporary stuff, that needs some final location / implementation

import Copilot.Core      hiding (Expr)
import Language.C99 as C hiding (Cast, And, Or, var)
import Language.C99.Util hiding (var)

import Data.Maybe (fromJust)
import Data.List  (find)

import GHC.Exts

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

  Index ty1@(Array ty2) -> (ty1, Word32, ty2)

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
  Cast _ _ -> "cast"
  GetField _ _ _ -> "field"


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
  Index    _   ->  "idx"


var :: String -> C.Expr
var n = wrap $ PrimIdent $ ident n

funcall :: String -> [AssignExpr] -> PostfixExpr
funcall name args = PostfixFunction (wrap $ var name) (args' args) where
  args' [] = Nothing
  args' xs = Just $ fromList xs

constint x = litint x
constword x = litint (fromIntegral x)
constfloat x = litfloat x
constdouble x = litdouble x
constbool True = PrimConst $ ConstEnum $ Enum (ident "true")
constbool False = PrimConst $ ConstEnum $ Enum (ident "false")



size_t = typedef' "size_t"


{-fundef :: String -> DeclnSpecs -> [Decln] -> CompoundStmt -> FunDef
fundef n ds args body = FunDef ds dr (args' args) body where
  dr = Declr Nothing (DirectDeclrIdent $ ident n)
  args' [] = Nothing
  args' [x] = Just $ DeclnBase x
  args' (x:xs)  = Just $ foldl (\xs x -> DeclnCons xs x) (DeclnBase x) xs-}

funarg ty name = Decln ty (Just $ InitDeclrBase $ InitDeclr $ (Declr Nothing (DirectDeclrIdent $ ident name)))




-- TODO
findstream :: Id -> [Stream] -> Stream
findstream id ss = fromJust $ find eq ss where
  eq s@(Stream id' _ _ _) = id == id'

-- Assign a variable
-- TODO
assign :: C.Expr -> C.Expr -> Stmt
assign var e = StmtExpr $ ExprStmt $ Just $ wrap $ Assign (wrap var) AEq (wrap e)


-- TODO
ddarray :: DirectDeclr -> [C.Expr] -> InitDeclr
ddarray dd xs = InitDeclrInitr varn (vals (map wrap xs)) where
  varn = Declr Nothing (DirectDeclrArray1 dd Nothing Nothing)
  vals (x:[]) = InitArray (InitBase Nothing (InitExpr x))
  vals (x:xs) = InitArray (foldl val base xs) where
    base = InitBase Nothing (InitExpr x)
    val xs x = InitCons xs Nothing (InitExpr x)
