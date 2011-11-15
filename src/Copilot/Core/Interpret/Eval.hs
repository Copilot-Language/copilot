--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | A tagless interpreter for Copilot specifications.

{-# LANGUAGE GADTs, BangPatterns #-}

module Copilot.Core.Interpret.Eval
  ( ExtEnv (..)
  , Env 
  , Output
  , ExecTrace (..)
  , eval
  ) where

import Copilot.Core
import Copilot.Core.Type.Dynamic
import Copilot.Core.Type.Show (showWithType, ShowType)

import Data.List (transpose)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromJust, catMaybes)
import Data.Bits

import Prelude hiding (id)
import qualified Prelude as P

--------------------------------------------------------------------------------

type Env nm = [(nm, DynamicF [] Type)]

-- | External arrays environment.
type ArrEnv = [(Name, [DynamicF [] Type])] 

-- | Environment for simulation.
data ExtEnv = ExtEnv { varEnv  :: Env Name
                     , arrEnv  :: ArrEnv 
                     , funcEnv :: [(Name, Spec)] 
                     }

--------------------------------------------------------------------------------

type Output = String

data ExecTrace = ExecTrace
    -- map from trigger names to their maybe output, which is a list of strings
    -- representing their values.  (Nothing output if the guard for the trigger
    -- is false).  The order is important, since we compare the arg lists
    -- between the interpreter and backends.
  { interpTriggers  :: Map String [Maybe [Output]]
    -- map from observer names to their outputs.  We also show observer outputs.
  , interpObservers :: Map String [Output] }
  deriving Show

--------------------------------------------------------------------------------

{-
eval :: Int -> Env Name -> Spec -> ExecTrace
eval k exts spec =
  let
    strms = fmap (evalStream     exts strms) (specStreams   spec)
    trigs = fmap (evalTrigger  k exts strms) (specTriggers  spec)
    obsvs = fmap (evalObserver k exts strms) (specObservers spec)
  in
    ExecTrace
      { interpTriggers  = M.fromList $
          zip (fmap triggerName  (specTriggers  spec)) trigs
      , interpObservers = M.fromList $
          zip (fmap observerName (specObservers spec)) obsvs
      }
-}

-- We could write this in a beautiful lazy style like above, but that creates a
-- space leak in the interpreter that is hard to fix while maintaining laziness.
-- We take a more brute-force appraoch below.
eval :: ShowType -> Int -> ExtEnv -> Spec -> ExecTrace
eval showType k exts spec =
--  let exts  = take k $ reverse exts'                          in

  let initStrms = map initStrm (specStreams spec)             in

  let strms = evalStreams k exts (specStreams spec) initStrms in

  let trigs = map (evalTrigger showType k exts strms) 
                  (specTriggers spec)                         in

  let obsvs = map (evalObserver showType k exts strms) 
                  (specObservers spec)                        in 

  strms `seq` ExecTrace
                { interpTriggers  = M.fromList $
                    zip (map triggerName  (specTriggers  spec)) trigs
                , interpObservers = M.fromList $
                    zip (map observerName (specObservers spec)) obsvs
                }

--------------------------------------------------------------------------------

type LocalEnv = [(Name, Dynamic Type)]

evalExpr_ :: Int -> Expr a -> ExtEnv -> LocalEnv -> Env Id -> a
evalExpr_ k e0 exts locs strms = case e0 of
  Const _ x              -> x 
  Drop t i id            -> 
    let Just xs = lookup id strms >>= fromDynF t in
    reverse xs !! (fromIntegral i + k)
  Local t1 _ name e1 e2 -> 
    let x     = evalExpr_ k e1 exts locs strms in
    let locs' = (name, toDyn t1 x) : locs  in
    x `seq` locs' `seq` evalExpr_ k e2 exts locs' strms
  Var t name                 -> fromJust $ lookup name locs >>= fromDyn t
  ExternVar t name           -> evalExtern k t name (varEnv exts)
  ExternFun t name _ _       -> evalFunc k t name exts
  ExternArray _ t name idx _ -> evalArray k t name evalIdx (arrEnv exts)
    where evalIdx = evalExpr_ k idx exts locs strms
  Op1 op e1              -> 
    let ev1 = evalExpr_ k e1 exts locs strms in 
    let op1 = evalOp1 op                     in
    ev1 `seq` op1 `seq` op1 ev1               
  Op2 op e1 e2           -> 
    let ev1 = evalExpr_ k e1 exts locs strms in 
    let ev2 = evalExpr_ k e2 exts locs strms in 
    let op2 = evalOp2 op                     in
    ev1 `seq` ev2 `seq` op2 `seq` op2 ev1 ev2
  Op3 op e1 e2 e3        -> 
    let ev1 = evalExpr_ k e1 exts locs strms in 
    let ev2 = evalExpr_ k e2 exts locs strms in 
    let ev3 = evalExpr_ k e3 exts locs strms in 
    let op3 = evalOp3 op                     in
    ev1 `seq` ev2 `seq` ev3 `seq` op3 `seq` op3 ev1 ev2 ev3

--------------------------------------------------------------------------------

evalExtern :: Int -> Type a -> Name -> Env Name -> a
evalExtern k t name exts = 
  case lookup name exts of
    Nothing -> badUsage $ "you need to supply a list of values for interpreting variable " ++ name
    Just dyn ->
      case fromDynF t dyn of
        Nothing -> badUsage $ "you probably gave the wrong type for external variable " ++ name ++ ".  Recheck your types and re-evaluate."
        Just xs -> xs !! k

--------------------------------------------------------------------------------

evalFunc :: Int -> Type a -> Name -> ExtEnv -> a
evalFunc k t name exts  = 
  case lookup name (funcEnv exts) of
    Nothing -> 
      badUsage $ "to simulate a spec containing the external function "
                   ++ name ++ ", you need to include a stream to simulate it"

    -- We created this spec in Interpreter.hs, copilot-language, so it should
    -- contain no triggers and exactly one observer.
    Just Spec { specStreams   = specStrms
              , specObservers = obsLs }  -> 
     let initStrms = map initStrm specStrms             in
     let strms = evalStreams k exts specStrms initStrms in
     case obsLs of
       [Observer { observerExpr     = expr_
                 , observerExprType = t1 }] -> 
         let dyn = toDynF t1 expr_ in
           case fromDynF t dyn of
             Nothing    -> impossible "evalFunc" "copilot-core"
             Just expr  -> evalExpr_ k expr exts [] strms
       _ -> badUsage $ "you probably gave the wrong type for external variable " ++ name ++ ".  Recheck your types and re-evaluate."

--------------------------------------------------------------------------------

evalArray :: Integral b => Int -> Type a -> Name -> b -> ArrEnv -> a
evalArray k t name idx exts =
  case lookup name exts of
    Nothing -> badUsage $ "you need to supply a list of finite lists " ++ 
                 "for interpreting array " ++ name
    Just dyn ->
      case catMaybes $ map (fromDynF t) dyn of
        [] -> badUsage $ "you probably gave the wrong type for external variable " ++ name ++ ".  Recheck your types and re-evaluate."
        xs -> let arr = (xs !! k) in
              if length arr > fromIntegral idx
                then arr !! fromIntegral idx
                else badUsage $ "in the environment for array " ++ name ++ 
                          ", you tried to index out of bounds"
  
--------------------------------------------------------------------------------

evalOp1 :: Op1 a b -> (a -> b)
evalOp1 op = case op of
  Not        -> P.not
  Abs _      -> P.abs
  Sign _     -> P.signum
  Recip _    -> P.recip
  Exp _      -> P.exp
  Sqrt _     -> P.sqrt
  Log _      -> P.log
  Sin _      -> P.sin
  Tan _      -> P.tan
  Cos _      -> P.cos
  Asin _     -> P.asin
  Atan _     -> P.atan
  Acos _     -> P.acos
  Sinh _     -> P.sinh
  Tanh _     -> P.tanh
  Cosh _     -> P.cosh
  Asinh _    -> P.asinh
  Atanh _    -> P.atanh
  Acosh _    -> P.acosh
  BwNot _    -> complement
  Cast _ _   -> P.fromIntegral 

--------------------------------------------------------------------------------

evalOp2 :: Op2 a b c -> (a -> b -> c)
evalOp2 op = case op of
  And          -> (&&)
  Or           -> (||)
  Add _        -> (+)
  Sub _        -> (-)
  Mul _        -> (*)
  Mod _        -> (catchZero P.mod)
  Div _        -> (catchZero P.div)
  Fdiv _       -> (P./)
  Pow _        -> (P.**)
  Logb _       -> P.logBase
  Eq _         -> (==)
  Ne _         -> (/=)
  Le _         -> (<=)
  Ge _         -> (>=)
  Lt _         -> (<)
  Gt _         -> (>)
  BwAnd _      -> (.&.)
  BwOr  _      -> (.|.)
  BwXor _      -> (xor)
  BwShiftL _ _ -> ( \ !a !b -> shiftL a $! fromIntegral b )
  BwShiftR _ _ -> ( \ !a !b -> shiftR a $! fromIntegral b )

catchZero :: Integral a => (a -> a -> a) -> (a -> a -> a)
catchZero _ _ 0 = badUsage "divide by zero"
catchZero f x y = f x y

--------------------------------------------------------------------------------

evalOp3 :: Op3 a b c d -> (a -> b -> c -> d)
evalOp3 (Mux _) = \ !v !x !y -> if v then x else y

--------------------------------------------------------------------------------

initStrm :: Stream -> (Id, DynamicF [] Type)
initStrm Stream { streamId       = id
                , streamBuffer   = buffer
                , streamExprType = t } =
  (id, toDynF t (reverse buffer))

-- XXX actually only need to compute until shortest stream is of length k
-- XXX this should just be a foldl' over [0,1..k]
evalStreams :: Int -> ExtEnv -> [Stream] -> Env Id -> Env Id
evalStreams top exts specStrms initStrms = 
  evalStreams_ 0 initStrms 
  where 
  evalStreams_ :: Int -> Env Id -> Env Id
  evalStreams_ k strms | k == top  = strms
  evalStreams_ k strms | otherwise = 
    evalStreams_ (k+1) $! strms_ 
    where 
    strms_ = map evalStream specStrms
    evalStream Stream { streamId       = id
                      , streamExpr     = e
                      , streamExprType = t } =
      let xs = fromJust $ lookup id strms >>= fromDynF t       in
      let x  = evalExpr_ k e exts [] strms                     in
      let ls = x `seq` (x:xs)                                  in
      (id, toDynF t ls)

--------------------------------------------------------------------------------

evalTrigger :: 
  ShowType -> Int -> ExtEnv -> Env Id -> Trigger -> [Maybe [Output]]
evalTrigger showType k exts strms
  Trigger
    { triggerGuard = e
    , triggerArgs  = args
    } = take k $ map tag (zip bs vs) ++ repeat Nothing -- there might be 0 args!

  where
  tag :: (Bool, a) -> Maybe a
  tag (True,  x) = Just x
  tag (False, _) = Nothing

  bs :: [Bool]
  bs = evalExprs_ k e exts strms

  vs :: [[Output]]
  vs = transpose $ map evalUExpr args 

  evalUExpr :: UExpr -> [Output]
  evalUExpr (UExpr t e1) =
    map (showWithType showType t) (evalExprs_ k e1 exts strms)

--------------------------------------------------------------------------------
evalObserver :: ShowType -> Int -> ExtEnv -> Env Id -> Observer -> [Output]
evalObserver showType k exts strms
  Observer
    { observerExpr     = e
    , observerExprType = t }
  = map (showWithType showType t) (evalExprs_ k e exts strms)

--------------------------------------------------------------------------------

evalExprs_ :: Int -> Expr a -> ExtEnv -> Env Id -> [a]
evalExprs_ k e exts strms = 
  map (\i -> evalExpr_ i e exts [] strms) [0..(k-1)]
                                       
