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

eval :: Int -> Env Name -> Spec -> ExecTrace
eval k exts spec =
  let
    strms = map (evalStream     exts strms) (specStreams   spec)
    trigs = map (evalTrigger  k exts strms) (specTriggers  spec)
    obsvs = map (evalObserver k exts strms) (specObservers spec)
  in
    ExecTrace
      { interpTriggers  = M.fromList $
          zip (map triggerName  (specTriggers  spec)) trigs
      , interpObservers = M.fromList $
          zip (map observerName (specObservers spec)) obsvs
      }

--------------------------------------------------------------------------------

evalConst x exts locs strms = x `seq` repeat x
evalDrop t i id exts locs strms = 
    let Just xs = lookup id strms >>= fromDynF t
    in  P.drop (fromIntegral i) xs
evalLocal t1 name e1 e2 exts locs strms = 
    let xs    = evalExpr_ e1 exts locs strms
        locs' = (name, toDynF t1 xs) : locs
    in  evalExpr_ e2 exts locs' strms
evalVar t name exts locs strms             = 
    let Just xs = lookup name locs >>= fromDynF t
    in  xs
evalExternVar t name exts locs strms       = evalExtern t name exts
evalExternArray =
    error "External arrays aren't supported in the interpreter"
evalExternFun =
    error "External functions aren't supported in the interpreter"
-- evalOp1' op e1 exts locs strms              = strictList $ repeat (evalOp1 op)
--                               <*> evalExpr_ e1 exts locs strms
-- evalOp2' op e1 e2 exts locs strms          =  strictList $ repeat (evalOp2 op)
--                              <*> evalExpr_ e1 exts locs strms
--                               <*> evalExpr_ e2 exts locs strms
-- evalOp3' op e1 e2 e3 exts locs strms       = strictList $ repeat (evalOp3 op)
--                               <*> evalExpr_ e1 exts locs strms
--                               <*> evalExpr_ e2 exts locs strms
--                               <*> evalExpr_ e3 exts locs strms
evalOp1' op e1 exts locs strms              =  map (evalOp1 op) (evalExpr_ e1 exts locs strms)

evalOp2' op e1 e2 exts locs strms          =   map (\(a,b) -> (evalOp2 op) a b)
                              (zip (evalExpr_ e1 exts locs strms)
                                 (evalExpr_ e2 exts locs strms))
evalOp3' op e1 e2 e3 exts locs strms       = map (\(a,b,c) -> (evalOp3 op) a b c)
                              (zip3 (evalExpr_ e1 exts locs strms)
                                   (evalExpr_ e2 exts locs strms)
                                   (evalExpr_ e3 exts locs strms))


evalExpr_ :: Expr a -> Env Name -> Env Name -> Env Id -> [a]
evalExpr_ e0 exts locs strms = case e0 of
  Const _ x              -> evalConst x exts locs strms
  Drop t i id            -> evalDrop t i id exts locs strms
  Local t1 _  name e1 e2 -> evalLocal t1 name e1 e2 exts locs strms
  Var t name             -> evalVar t name exts locs strms
  ExternVar t name       -> evalExternVar t name exts locs strms
  ExternArray _ _ _ _    -> evalExternArray 
  ExternFun _ _ _         -> evalExternFun 
  Op1 op e1              -> evalOp1' op e1 exts locs strms
  Op2 op e1 e2           -> evalOp2'  op e1 e2 exts locs strms
  Op3 op e1 e2 e3        -> evalOp3'  op e1 e2 e3 exts locs strms


-- (<*>) :: [(a -> b)] -> [a] -> [b]
-- a <*> b = zipWith ($) a b


-- zipWith' f l1 l2 = 
--   [ f e1 e2 | (e1, e2) <- zipWith k l1 l2 ]
--   where
--   k x y = x `seq` y `seq` (x,y)


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
--   ExternArray _ _ _ _    ->
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
-- >>>>>>> master

evalExtern :: Type a -> Name -> Env Name -> [a]
evalExtern t name exts =
  case lookup name exts of
    Nothing -> error $ "Undefined external variable: " ++ name
    Just dyn ->
      case fromDynF t dyn of
        Nothing -> error $ "Ill-typed external variable: " ++ name
        Just xs -> xs

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

evalStream :: Env Name -> Env Id -> Stream -> (Int, DynamicF [] Type)
evalStream exts strms
  Stream
    { streamId       = id
    , streamBuffer   = buffer
    , streamExpr     = e
    , streamExprType = t
--    , streamGuard    = g
    } = (id, toDynF t xs)

  where

  xs = buffer ++ evalExpr_ e exts [] strms
--  ys = withGuard (uninitialized t) (evalExpr_ g exts [] strms) xs

{-
  withGuard :: a -> [Bool] -> [a] -> [a]
  withGuard _ (True:vs)  (z:zs) = z : withGuard z vs zs
  withGuard z (False:vs) zs     = z : withGuard z vs zs
  withGuard _ _          _      = []
-}
--------------------------------------------------------------------------------

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
  bs = evalExpr_ e exts [] strms

  vs :: [[Output]]
  vs = transpose $ map evalUExpr args

  evalUExpr :: UExpr -> [Output]
  evalUExpr (UExpr t e1) =
    map (showWithType t) (evalExpr_ e1 exts [] strms)

--------------------------------------------------------------------------------


evalObserver :: Int -> Env Name -> Env Id -> Observer -> [Output]
evalObserver k exts strms
  Observer
    { observerExpr     = e
    , observerExprType = t }
  = take k $ map (showWithType t) (evalExpr_ e exts [] strms)

--------------------------------------------------------------------------------
