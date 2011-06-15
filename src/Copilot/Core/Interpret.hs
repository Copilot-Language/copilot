--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | An tagless interpreter for Copilot specifications.

module Copilot.Core.Interpret
  ( Env
  , interpret
  ) where

import Data.List (intersperse, transpose)
import Copilot.Core
import Copilot.Core.Type.Dynamic
import Copilot.Core.Type.Show (showWithType)
import Prelude hiding (id)
import qualified Prelude as P
import Text.PrettyPrint.NCol (asColumns)
import Text.PrettyPrint (Doc, text, render)

--------------------------------------------------------------------------------

type Env k = [(k, DynamicF [] Type)]

--------------------------------------------------------------------------------

strictList :: [α] -> [α]
strictList []     = []
strictList (x:xs) = x `seq` (x : strictList xs)

strictEval :: EvalExpr α -> EvalExpr α
strictEval e = e `seq` EvalExpr $
  \ exts lets strms -> strictList (evalExpr_ e exts lets strms)

--------------------------------------------------------------------------------

class Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

--------------------------------------------------------------------------------

newtype EvalExpr a = EvalExpr
  { evalExpr_ :: Env Name -> Env Name -> Env Id -> [a] }

instance Applicative EvalExpr where
  pure x    = EvalExpr $ \ _ _ _ -> repeat x
  e1 <*> e2 = EvalExpr $ \ exts lets strms -> e1 `seq` e2 `seq`
    zipWith ($) (evalExpr_ e1 exts lets strms) (evalExpr_ e2 exts lets strms)

instance Expr EvalExpr where
  const _ x              = x `seq` pure x
  drop t i id            = EvalExpr $ \ _ _ strms -> strictList $
                             let
                               Just xs = lookup id strms >>= fromDynamicF t
                             in
                               P.drop (fromIntegral i) xs
  local t1 _  name e1 e2 = EvalExpr $ \ exts lets strms -> strictList $
                             let
                               xs    = evalExpr_ e1 exts lets strms
                               lets' = (name, toDynamicF xs t1) : lets
                             in
                               evalExpr_ e2 exts lets' strms
  var t name             = EvalExpr $ \ _ lets _ -> strictList $
                             let
                               Just xs = lookup name lets >>= fromDynamicF t
                             in
                               xs
  extern t name          = EvalExpr $ \ exts _ _ -> strictList $
                             evalExtern t name exts
  op1 op e1              = strictEval $ pure op <*> e1
  op2 op e1 e2           = strictEval $ pure (apply2 op) <*> e1 <*> e2
  op3 op e1 e2 e3        = strictEval $ pure (apply3 op) <*> e1 <*> e2 <*> e3

evalExtern :: Type a -> Name -> Env Name -> [a]
evalExtern t name exts =
  case lookup name exts of
    Nothing -> error $ "Undefined external variable: " ++ name
    Just dyn ->
      case fromDynamicF t dyn of
        Nothing -> error $ "Ill-typed external variable: " ++ name
        Just xs -> xs

--------------------------------------------------------------------------------

instance Op1 (->) where
  not      = P.not
  abs _    = P.abs
  sign _   = P.signum
  recip _  = P.recip
  exp _    = P.exp
  sqrt _   = P.sqrt
  log _    = P.log
  sin _    = P.sin
  tan _    = P.tan
  cos _    = P.cos
  asin _   = P.asin
  atan _   = P.atan
  acos _   = P.acos
  sinh _   = P.sinh
  tanh _   = P.tanh
  cosh _   = P.cosh
  asinh _  = P.asinh
  atanh _  = P.atanh
  acosh _  = P.acosh

--------------------------------------------------------------------------------

newtype Apply2 a b c = Apply2 { apply2 :: a -> b -> c }

instance Op2 Apply2 where
  and      = Apply2 (&&)
  or       = Apply2 (||)
  add _    = Apply2 (+)
  sub _    = Apply2 (-)
  mul _    = Apply2 (*)
  mod _    = Apply2 P.mod
  div _    = Apply2 P.div
  fdiv _   = Apply2 (P./)
  pow _    = Apply2 (P.**)
  logb _   = Apply2 P.logBase
  eq _     = Apply2 (==)
  ne _     = Apply2 (/=)
  le _     = Apply2 (<=)
  ge _     = Apply2 (>=)
  lt _     = Apply2 (<)
  gt _     = Apply2 (>)

--------------------------------------------------------------------------------

newtype Apply3 a b c d = Apply3 { apply3 :: a -> b -> c -> d }

instance Op3 Apply3 where
  mux _    = Apply3 $ \ v x y -> if v then x else y

--------------------------------------------------------------------------------

type Output = (Bool, [String])

--------------------------------------------------------------------------------

evalStream :: Env Name -> Env Name -> Env Id -> Stream -> (Int, DynamicF [] Type)
evalStream exts lets strms
  Stream
    { streamId       = id
    , streamBuffer   = buffer
    , streamExpr     = e
    , streamExprType = t
    } = (id, toDynamicF xs t)

  where

  xs = strictList $ buffer ++ evalExpr_ e exts lets strms
--  ys =
--    case mguard of
--      Just e2 -> withGuard (uninitialized t) (evalExpr env e2) xs
--      Nothing -> xs

--  withGuard :: a -> [Bool] -> [a] -> [a]
--  withGuard _ (True:vs)  (x:xs) = x : withGuard x vs xs
--  withGuard z (False:vs) xs     = z : withGuard z vs xs
--  withGuard _ _          _      = error "withGuard: empty stream."

--------------------------------------------------------------------------------

evalTrigger :: Env Name -> Env Name -> Env Id -> Trigger -> [Output]
evalTrigger exts lets strms
  Trigger
    { triggerGuard = e
    , triggerArgs  = args
    } = zip bs vs

  where

  bs :: [Bool]
  bs = evalExpr_ e exts lets strms

  vs :: [[String]]
  vs = transpose $ map evalTriggerArg args

  evalTriggerArg :: TriggerArg -> [String]
  evalTriggerArg (TriggerArg e1 t) =
    map (showWithType t) (evalExpr_ e1 exts lets strms)

--------------------------------------------------------------------------------

evalLet :: Env Name -> Env Name -> Env Id -> Let -> (Name, DynamicF [] Type)
evalLet exts lets strms
  Let
    { letExpr = e
    , letVar = name
    , letType = t
    } = (name, toDynamicF xs t)
  where
    xs = strictList $ evalExpr_ e exts lets strms

--------------------------------------------------------------------------------

evalSpec :: Env Name -> Spec -> [[Output]]
evalSpec exts spec = outps
  where
    strms = fmap (evalStream  exts [] strms) (specStreams  spec)
    outps = fmap (evalTrigger exts [] strms) (specTriggers spec)

--------------------------------------------------------------------------------

-- | Interprets a Copilot specification.
interpret :: Int -> Env Name -> Spec -> String
interpret k exts spec =
  ( render
  . asColumns
  . transpose
  . (:) (ppTriggerNames $ specTriggers spec)
  . take k
  . ppOutputs
  . evalSpec exts
  ) spec

--------------------------------------------------------------------------------

-- Functions for pretty-printing the output of the interpreter:

ppTriggerNames :: [Trigger] -> [Doc]
ppTriggerNames = map (text . (++ ":") . triggerName)

ppOutput :: Output -> Doc
ppOutput (True,  vs) = text $ "(" ++ concat (intersperse "," vs) ++ ")"
ppOutput (False, _ ) = text "--"

ppOutputs :: [[Output]] -> [[Doc]]
ppOutputs [] = []
ppOutputs xs = map (ppOutput . head) xs : ppOutputs (map tail xs)

--------------------------------------------------------------------------------
