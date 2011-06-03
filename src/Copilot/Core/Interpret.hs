-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | An tagless interpreter for Copilot specifications.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Interpret
  ( Env
  , interpret
  ) where

import Data.List (intersperse, transpose)
import Copilot.Core
import Copilot.Core.Type.Dynamic
import Prelude hiding (id)
import qualified Prelude as P
import Text.PrettyPrint.NCol (asColumns)
import Text.PrettyPrint (Doc, text, render)

--------------------------------------------------------------------------------

type Env k = [(k, DynamicF [] Type)]

--------------------------------------------------------------------------------

class Strict f where
  strict :: f α -> f α

instance Strict [] where
  strict []     = []
  strict (x:xs) = x `seq` (x : strict xs)

--------------------------------------------------------------------------------

class Applicative f where
  pure  :: α -> f α
  (<*>) :: f (α -> β) -> f α -> f β

--------------------------------------------------------------------------------

newtype EvalExpr α = EvalExpr { evalExpr_ :: Env Name -> Env Id -> [α] }

instance Applicative EvalExpr where
  pure x    = EvalExpr $ \ _ _ -> repeat x
  e1 <*> e2 = EvalExpr $ \ exts strms ->
    zipWith ($) (evalExpr_ e1 exts strms) (evalExpr_ e2 exts strms)

instance Strict EvalExpr where
  strict e = EvalExpr $ \ exts strms -> strict (evalExpr_ e exts strms)

instance Expr EvalExpr where
  const _ x       = x `seq` pure x
  drop t i id     = strict $ EvalExpr $ \ _ strms ->
                      let Just xs = lookup id strms >>= fromDynamicF t
                      in P.drop (fromIntegral i) xs
  extern t name   = strict $ EvalExpr $ \ exts _ -> evalExtern t name exts
  op1 op e1       = strict $ pure op <*> e1
  op2 op e1 e2    = strict $ pure (apply2 op) <*> e1 <*> e2
  op3 op e1 e2 e3 = strict $ pure (apply3 op) <*> e1 <*> e2 <*> e3

evalExtern :: Type α -> Name -> Env Name -> [α]
evalExtern t name exts =
  case lookup name exts of
    Nothing -> error $ "Undefined external variable: " ++ name
    Just dyn ->
      case fromDynamicF t dyn of
        Nothing -> error $ "Ill-typed external variable: " ++ name
        Just xs -> xs

evalExpr :: Env Name -> Env Id -> (forall e . Expr e => e α) -> [α]
evalExpr exts strms (EvalExpr f) = f exts strms

--------------------------------------------------------------------------------

instance Op1 (->) where
  not      = P.not
  abs _    = P.abs
  sign _   = P.signum

--------------------------------------------------------------------------------

newtype Apply2 α β γ = Apply2 { apply2 :: α -> β -> γ }

instance Op2 Apply2 where
  and      = Apply2 (&&)
  or       = Apply2 (||)
  add _    = Apply2 (+)
  sub _    = Apply2 (-)
  mul _    = Apply2 (*)
  mod _    = Apply2 P.mod
  div _    = Apply2 P.div
  eq _     = Apply2 (==)
  ne _     = Apply2 (/=)
  le _     = Apply2 (<=)
  ge _     = Apply2 (>=)
  lt _     = Apply2 (<)
  gt _     = Apply2 (>)

--------------------------------------------------------------------------------

newtype Apply3 α β γ δ = Apply3 { apply3 :: α -> β -> γ -> δ }

instance Op3 Apply3 where
  mux _    = Apply3 $ \ v x y -> if v then x else y

--------------------------------------------------------------------------------

type Output = (Bool, [String])

--------------------------------------------------------------------------------

evalStream :: Env Name -> Env Id -> Stream -> (Int, DynamicF [] Type)
evalStream exts strms (Stream id buffer _ e2 t) = (id, toDynamicF xs t)

  where

  xs = buffer ++ evalExpr exts strms e2
--  ys =
--    case mguard of
--      Just e2 -> withGuard (uninitialized t) (evalExpr env e2) xs
--      Nothing -> xs

--  withGuard :: α -> [Bool] -> [α] -> [α]
--  withGuard _ (True:vs)  (x:xs) = x : withGuard x vs xs
--  withGuard z (False:vs) xs     = z : withGuard z vs xs
--  withGuard _ _          _      = error "withGuard: empty stream."

--------------------------------------------------------------------------------

evalTrigger :: Env Name -> Env Id -> Trigger -> [Output]
evalTrigger exts strms (Trigger _ e args) = zip bs vs

  where

  bs :: [Bool]
  bs = evalExpr exts strms e

  vs :: [[String]]
  vs = transpose $ map evalTriggerArg args

  evalTriggerArg :: TriggerArg -> [String]
  evalTriggerArg (TriggerArg e1 _) = map show (evalExpr exts strms e1)

--------------------------------------------------------------------------------

evalSpec :: Env Name -> Spec -> [[Output]]
evalSpec exts spec = outps
  where
    strms = fmap (evalStream  exts strms) (specStreams  spec)
    outps = fmap (evalTrigger exts strms) (specTriggers spec)

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