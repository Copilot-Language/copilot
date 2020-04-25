module Copilot.Compile.C99.Test.Driver where

import Language.C99.Simple

import Copilot.Core       as Core
  ( Spec    (..)
  , UExpr   (..)
  , Trigger (..)
  )
import Copilot.Compile.C99.Translate  (transtype)
import Copilot.Compile.C99.Util       (argnames)


-- | Write a C driver mimicking Copilot's interpreter.
mkdriver :: Spec -> TransUnit
mkdriver spec = TransUnit vardefs fundefs
  where
    vardefs   = []
    fundefs   = ctriggers ++ [ mkmain 30 spec ]
    ctriggers = map mktrigger (specTriggers spec)


-- | Write a trigger function which prints all its arguments in a CSV like
-- format.
mktrigger :: Trigger -> FunDef
mktrigger (Trigger name guard args) = FunDef returntype name params [] body
  where
    returntype = TypeSpec Void
    namedargs  = zip (argnames name) args
    params     = map mkparam namedargs
    body       = [Expr $ mkprintfcsv namedargs]

    mkparam :: (String, UExpr) -> Param
    mkparam (name, UExpr ty _) = Param (transtype ty) name


-- | Write the main function. The purpose of this function is to call the
-- step-function a number of times.
mkmain :: Int -> Spec -> FunDef
mkmain iters spec = FunDef (TypeSpec Int) "main" params decln body
  where
    params = [ Param (TypeSpec Int) "argv"
             , Param (Const $ Array (Ptr $ TypeSpec Char) Nothing) "argc"
             ]
    decln  = [VarDecln Nothing (TypeSpec Int) "i" (Just $ InitExpr $ LitInt 0)]
    body   = [For (Ident "i" .= LitInt 0)
                  (Ident "i" .< LitInt (fromIntegral iters))
                  (UnaryOp Inc (Ident "i"))
                  [Expr $ Funcall (Ident "step") []]
             ]


-- | Write a call to printf with a format resembling CSV.
mkprintfcsv :: [(String, UExpr)] -> Expr
mkprintfcsv = error "mkprintfcsv not implemented yet."
