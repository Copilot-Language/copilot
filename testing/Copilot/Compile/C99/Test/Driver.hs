module Copilot.Compile.C99.Test.Driver where

import Language.C99.Simple

import Copilot.Core       as Core     (Spec (..))


-- | Write a C driver mimicking Copilot's interpreter.
mkdriver :: Spec -> TransUnit
mkdriver spec = TransUnit vardefs fundefs
  where
    vardefs = []
    fundefs = [ mkmain 30 spec ]


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
