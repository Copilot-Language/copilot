--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | An tagless interpreter for Copilot specifications.

{-# LANGUAGE GADTs #-}

module Copilot.Core.Interpret.Eval
  ( Env
  , Output
  , ExecTrace (..)
  , eval
  ) where

import Copilot.Core
import Copilot.Core.Type.Dynamic
import Copilot.Core.Type.Show (showWithType)
import Data.List (transpose)
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Bits
import Prelude hiding (id)
import qualified Prelude as P

--------------------------------------------------------------------------------

type Env k = [(k, DynamicF [] Type)]

--------------------------------------------------------------------------------

type Output = String

data ExecTrace = ExecTrace
  { interpTriggers  :: Map String [Maybe [Output]]
  , interpObservers :: Map String [Output] }
  deriving Show

--------------------------------------------------------------------------------

-- eval :: Int -> Env Name -> Spec -> ExecTrace
-- eval k exts spec =
--   let
--     strms = fmap (evalStream     exts strms) (specStreams   spec)
--     trigs = fmap (evalTrigger  k exts strms) (specTriggers  spec)
--     obsvs = fmap (evalObserver k exts strms) (specObservers spec)
--   in
--     ExecTrace
--       { interpTriggers  = M.fromList $
--           zip (fmap triggerName  (specTriggers  spec)) trigs
--       , interpObservers = M.fromList $
--           zip (fmap observerName (specObservers spec)) obsvs
--       }

-- We could write this in a beautiful lazy style like above, but that creates a
-- space leak in the interpreter that is hard to fix while maintaining laziness.
-- We take a more brute-force appraoch below.
eval :: Int -> Env Name -> Spec -> ExecTrace
eval k exts spec =
  let initStrm Stream { streamId       = id
                      , streamBuffer   = buffer
                      , streamExprType = t } =
        (id, toDynF t buffer)                                 in

  let initStrms = map initStrm (specStreams spec)             in

  let strms = evalStreams k exts (specStreams spec) initStrms in

  let trigs = strms `seq` map (evalTrigger  k exts strms) 
                              (specTriggers  spec)            in

  let obsvs = strms `seq` map (evalObserver k exts strms) 
                              (specObservers spec)            in 
  ExecTrace
    { interpTriggers  = M.fromList $
        zip (fmap triggerName  (specTriggers  spec)) trigs
    , interpObservers = M.fromList $
        zip (fmap observerName (specObservers spec)) obsvs
    }

--------------------------------------------------------------------------------

-- evalExpr_ :: Expr a -> Env Name -> Env Name -> Env Id -> [a]
-- evalExpr_ e0 exts locs strms = case e0 of
--   Const _ x              -> x `seq` repeat x
--   Drop t i id            -> 
--     let Just xs = lookup id strms >>= fromDynF t
--     in  P.drop (fromIntegral i) xs
--   Local t1 _  name e1 e2 -> 
--     let xs    = evalExpr_ e1 exts locs strms
--         locs' = (name, toDynF t1 xs) : locs
--     in  evalExpr_ e2 exts locs' strms
--   Var t name             -> 
--     let Just xs = lookup name locs >>= fromDynF t
--     in  xs
--   ExternVar t name       -> evalExtern t name exts
--   ExternArray _ _ _ _ _    ->
--     error "External arrays aren't supported in the interpreter"
--   ExternFun _ _ _ _      ->
--     error "External functions aren't supported in the interpreter"
--   Op1 op e1              -> map (evalOp1 op)
--                                 (evalExpr_ e1 exts locs strms)
--   Op2 op e1 e2           -> map (\(a,b) -> (evalOp2 op) a b) $ 
--                                 zip (evalExpr_ e1 exts locs strms)
--                                     (evalExpr_ e2 exts locs strms)
--   Op3 op e1 e2 e3        -> map (\(a,b,c) -> (evalOp3 op) a b c) $
--                                 zip3 (evalExpr_ e1 exts locs strms)
--                                      (evalExpr_ e2 exts locs strms)
--                                      (evalExpr_ e3 exts locs strms)

type LocalEnv = [(Name, Dynamic Type)]

evalExpr_ :: Int -> Expr a -> Env Name -> LocalEnv -> Env Id -> a
evalExpr_ k e0 exts locs strms = case e0 of
  Const _ x              -> x 
  Drop t i id            -> 
    let Just xs = lookup id strms >>= fromDynF t in
    xs !! (fromIntegral i + k)
  Local t1 _ name e1 e2 -> 
    let x     = evalExpr_ k e1 exts locs strms in
    let locs' = (name, toDyn t1 x) : locs  in
    evalExpr_ k e2 exts locs' strms
  Var t name             -> fromJust $ lookup name locs >>= fromDyn t
  ExternVar t name       -> evalExtern k t name exts
  ExternArray _ _ _ _ _  ->
    error "External arrays aren't supported in the interpreter"
  ExternFun _ _ _ _      ->
    error "External functions aren't supported in the interpreter"
  Op1 op e1              -> (evalOp1 op) (evalExpr_ k e1 exts locs strms)
  Op2 op e1 e2           -> (evalOp2 op) 
                              (evalExpr_ k e1 exts locs strms)
                              (evalExpr_ k e2 exts locs strms)
  Op3 op e1 e2 e3        -> (evalOp3 op)
                              (evalExpr_ k e1 exts locs strms)
                              (evalExpr_ k e2 exts locs strms)
                              (evalExpr_ k e3 exts locs strms)

-- evalExtern :: Type a -> Name -> Env Name -> [a]
-- evalExtern t name exts =
--   case lookup name exts of
--     Nothing -> error $ "Undefined external variable: " ++ name
--     Just dyn ->
--       case fromDynF t dyn of
--         Nothing -> error $ "Ill-typed external variable: " ++ name
--         Just xs -> xs
evalExtern :: Int -> Type a -> Name -> Env Name -> a
evalExtern k t name exts =
  case lookup name exts of
    Nothing -> error $ "Undefined external variable: " ++ name
    Just dyn ->
      case fromDynF t dyn of
        Nothing -> error $ "Ill-typed external variable: " ++ name
        Just xs -> xs !! k

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
  BwShiftL _ _ -> ( \ a b -> shiftL a $ fromIntegral b )
  BwShiftR _ _ -> ( \ a b -> shiftR a $ fromIntegral b )

catchZero :: Integral a => (a -> a -> a) -> (a -> a -> a)
catchZero _ _ 0 = error "divide by zero"
catchZero f x y = f x y

--------------------------------------------------------------------------------

evalOp3 :: Op3 a b c d -> (a -> b -> c -> d)
evalOp3 (Mux _) = \ v x y -> if v then x else y

--------------------------------------------------------------------------------

-- evalStream :: Env Name -> Env Id -> Stream -> (Int, DynamicF [] Type)
-- evalStream exts strms
--   Stream
--     { streamId       = id
--     , streamBuffer   = buffer
--     , streamExpr     = e
--     , streamExprType = t
--     } = (id, toDynF t xs)

--   where

--   xs = buffer ++ evalExpr_ e exts [] strms

-- XXX actually only need to compute until shortest stream is of length k
-- XXX this should just be a foldl' over [0,1..k]
evalStreams_ :: Int -> Env Name -> [Stream] -> Env Id -> Env Id
evalStreams_ 0 _    _         strms = strms
evalStreams k exts specStrms strms = 
  evalStreams (k-1) exts specStrms strms_ 
  where 
  strms_ = map evalStream specStrms
  evalStream Stream { streamId       = id
                    , streamBuffer   = buffer
                    , streamExpr     = e
                    , streamExprType = t } =
    let xs = fromJust $ (lookup id strms >>= fromDynF t) in
    (id, toDynF t (xs ++ [x]))
    where x = evalExpr_ k e exts [] strms

--------------------------------------------------------------------------------

-- evalTrigger :: Int -> Env Name -> Env Id -> Trigger -> [Maybe [Output]]
-- evalTrigger k exts strms
--   Trigger
--     { triggerGuard = e
--     , triggerArgs  = args
--     } = take k $ map tag (zip bs vs)

--   where
--   tag :: (Bool, a) -> Maybe a
--   tag (True,  x) = Just x
--   tag (False, _) = Nothing

--   bs :: [Bool]
--   bs = evalExpr_ e exts [] strms

--   vs :: [[Output]]
--   vs = transpose $ map evalUExpr args

--   evalUExpr :: UExpr -> [Output]
--   evalUExpr (UExpr t e1) =
--     map (showWithType t) (evalExpr_ e1 exts [] strms)
evalTrigger :: Int -> Env Name -> Env Id -> Trigger -> [Maybe [Output]]
evalTrigger k exts strms
  Trigger
    { triggerGuard = e
    , triggerArgs  = args
    } = take k $ map tag (zip bs vs)

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
    map (showWithType t) (evalExprs_ k e1 exts strms)

--------------------------------------------------------------------------------


-- evalObserver :: Int -> Env Name -> Env Id -> Observer -> [Output]
-- evalObserver k exts strms
--   Observer
--     { observerExpr     = e
--     , observerExprType = t }
--   = take k $ map (showWithType t) (evalExpr_ e exts [] strms)
evalObserver :: Int -> Env Name -> Env Id -> Observer -> [Output]
evalObserver k exts strms
  Observer
    { observerExpr     = e
    , observerExprType = t }
  = take k $ map (showWithType t) (evalExprs_ k e exts strms)

--------------------------------------------------------------------------------

evalExprs_ :: Int -> Expr a -> Env Name -> Env Id -> [a]
evalExprs_ k e exts strms = 
  map (\i -> evalExpr_ i e exts [] strms) [0..k]
                                       
