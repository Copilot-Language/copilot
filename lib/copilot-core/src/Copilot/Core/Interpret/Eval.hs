--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | A tagless interpreter for Copilot specifications.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs, BangPatterns, DeriveDataTypeable #-}

module Copilot.Core.Interpret.Eval
  ( Env
  , Output
  , ExecTrace (..)
  , eval
  ) where

import Copilot.Core
import Copilot.Core.Type.Show (showWithType, ShowType)

import Prelude hiding (id)
import qualified Prelude as P

import Data.List (transpose)
import Data.Maybe (fromJust)
import Data.Bits
import Control.Exception (Exception, throw)

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Typeable (Typeable)

--------------------------------------------------------------------------------

-- | Exceptions that may be thrown during interpretation of a Copilot
-- specification.
data InterpException
  = ArrayWrongSize Name Int           -- ^ Extern array has incorrect size.
  | ArrayIdxOutofBounds Name Int Int  -- ^ Index out-of-bounds exception.
  | DivideByZero                      -- ^ Division by zero.
  | NotEnoughValues Name Int          -- ^ For one or more streams, not enough
                                      --   values are available to simulate the
                                      --   number of steps requested.
  | NoExtsInterp Name                 -- ^ One of the externs used by the
                                      --   specification does not declare
                                      --   sample values to be used during
                                      --   simulation.
  deriving Typeable

-- | Show a descriptive message of the exception.
instance Show InterpException where
  ---------------------------------------
  show (ArrayWrongSize name expectedSize)                                      =
    badUsage $ "in the environment for external array " ++ name
      ++ ", we expect a list of length " ++ show expectedSize
      ++ ", but the length of the array you supplied is of a different length."
  ---------------------------------------
  show (ArrayIdxOutofBounds name index size)                                   =
    badUsage $ "in the environment for external array " ++ name
      ++ ", you gave an index of " ++ show index
      ++ " where the size of the array is " ++ show size ++ "; the size must "
      ++ " be strictly greater than the index."
  ---------------------------------------
  show DivideByZero                                                            =
    badUsage "divide by zero."
  ---------------------------------------
  show (NotEnoughValues name k)                                                =
    badUsage $ "on the " ++ show k ++ "th iteration, we ran out of "
      ++ "values for simulating the external element " ++ name ++ "."
  ---------------------------------------
  show (NoExtsInterp name)                                                     =
    badUsage $ "in a call of external symbol " ++ name ++ ", you did not "
      ++ "provide an expression for interpretation.  In your external "
      ++ "declaration, you need to provide a 'Just strm', where 'strm' is "
      ++ "some stream with which to simulate the function."
  ---------------------------------------

-- | Allow throwing and catching 'InterpException' using Haskell's standard
-- exception mechanisms.
instance Exception InterpException

--------------------------------------------------------------------------------

-- | An environment that contains an association between (stream or extern)
-- names and their values.
type Env nm = [(nm, Dynamic)]

--------------------------------------------------------------------------------

-- | The simulation output is defined as a string. Different backends may
-- choose to format their results differently.
type Output = String

-- | An execution trace, containing the traces associated to each individual
-- monitor trigger and observer.
data ExecTrace = ExecTrace
  { interpTriggers  :: [(String, [Maybe [Output]])]

    -- ^ Map from trigger names to their optional output, which is a list of
    -- strings representing their values. The output may be 'Nothing' if the
    -- guard for the trigger was false. The order is important, since we
    -- compare the arg lists between the interpreter and backends.

  , interpObservers :: [(String, [Output])]
    -- ^ Map from observer names to their outputs.
  }
  deriving Show

--------------------------------------------------------------------------------

-- We could write this in a beautiful lazy style like above, but that creates a
-- space leak in the interpreter that is hard to fix while maintaining laziness.
-- We take a more brute-force appraoch below.

-- | Evaluate a specification for a number of steps.
eval :: ShowType   -- ^ Show booleans as @0@\/@1@ (C) or @True@\/@False@
                   --   (Haskell).
     -> Int        -- ^ Number of steps to evaluate.
     -> Spec       -- ^ Specification to evaluate.
     -> ExecTrace
eval showType k spec =

  let initStrms = map initStrm (specStreams spec)             in

  let strms = evalStreams k (specStreams spec) initStrms      in

  let trigs = map (evalTrigger showType k strms)
                  (specTriggers spec)                         in

  let obsvs = map (evalObserver showType k strms)
                  (specObservers spec)                        in

  strms `seq` ExecTrace
                { interpTriggers  =
                    zip (map triggerName  (specTriggers  spec)) trigs
                , interpObservers =
                    zip (map observerName (specObservers spec)) obsvs
                }

--------------------------------------------------------------------------------

-- | An environment that contains an association between (stream or extern)
-- names and their values.
type LocalEnv = [(Name, Dynamic)]

-- | Evaluate an expression for a number of steps, obtaining the value
-- of the sample at that time.
evalExpr_ :: Typeable a => Int -> Expr a -> LocalEnv -> Env Id -> a
evalExpr_ k e0 locs strms = case e0 of
  Const _ x                          -> x
  Drop t i id                        ->
    let Just buff = lookup id strms >>= fromDynamic in
    reverse buff !! (fromIntegral i + k)
  Local t1 _ name e1 e2              ->
    let x     = evalExpr_ k e1 locs strms in
    let locs' = (name, toDyn x) : locs  in
    x `seq` locs' `seq` evalExpr_ k e2  locs' strms
  Var t name                         -> fromJust $ lookup name locs >>= fromDynamic
  ExternVar _ name xs                -> evalExternVar k name xs
  Op1 op e1                          ->
    let ev1 = evalExpr_ k e1 locs strms in
    let op1 = evalOp1 op                in
    ev1 `seq` op1 `seq` op1 ev1
  Op2 op e1 e2                       ->
    let ev1 = evalExpr_ k e1 locs strms in
    let ev2 = evalExpr_ k e2 locs strms in
    let op2 = evalOp2 op                in
    ev1 `seq` ev2 `seq` op2 `seq` op2 ev1 ev2
  Op3 op e1 e2 e3                    ->
    let ev1 = evalExpr_ k e1 locs strms in
    let ev2 = evalExpr_ k e2 locs strms in
    let ev3 = evalExpr_ k e3 locs strms in
    let op3 = evalOp3 op                in
    ev1 `seq` ev2 `seq` ev3 `seq` op3 `seq` op3 ev1 ev2 ev3
  Label _ _ e1                         ->
    let ev1 = evalExpr_ k e1 locs strms in
    ev1

--------------------------------------------------------------------------------

-- | Evaluate an extern stream for a number of steps, obtaining the value of
-- the sample at that time.
evalExternVar :: Int -> Name -> Maybe [a] -> a
evalExternVar k name exts =
  case exts of
    Nothing -> throw (NoExtsInterp name)
    Just xs ->
      case safeIndex k xs of
        Nothing -> throw (NotEnoughValues name k)
        Just x  -> x

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | Evaluate an 'Copilot.Core.Operators.Op1' by producing an equivalent
-- Haskell function operating on the same types as the
-- 'Copilot.Core.Operators.Op1'.
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
  GetField (Struct _) _ f -> unfield . f where
    unfield (Field v) = v

--------------------------------------------------------------------------------

-- | Evaluate an 'Copilot.Core.Operators.Op2' by producing an equivalent
-- Haskell function operating on the same types as the
-- 'Copilot.Core.Operators.Op2'.
evalOp2 :: Op2 a b c -> (a -> b -> c)
evalOp2 op = case op of
  And          -> (&&)
  Or           -> (||)
  Add _        -> (+)
  Sub _        -> (-)
  Mul _        -> (*)
  Mod _        -> (catchZero P.mod)
  Div _        -> (catchZero P.quot)
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
  Index    _   -> \xs n -> (arrayelems xs) !! (fromIntegral n)

-- | Apply a function to two numbers, so long as the second one is
-- not zero.
--
-- Used to detect attempts at dividing by zero and produce the appropriate
-- 'InterpException'.
catchZero :: Integral a => (a -> a -> a) -> (a -> a -> a)
catchZero _ _ 0 = throw DivideByZero
catchZero f x y = f x y

--------------------------------------------------------------------------------

-- | Evaluate an 'Copilot.Core.Operators.Op3' by producing an equivalent
-- Haskell function operating on the same types as the
-- 'Copilot.Core.Operators.Op3'.
evalOp3 :: Op3 a b c d -> (a -> b -> c -> d)
evalOp3 (Mux _) = \ !v !x !y -> if v then x else y

--------------------------------------------------------------------------------

-- | Turn a stream into a key-value pair that can be added to an 'Env' for
-- simulation.
initStrm :: Stream -> (Id, Dynamic)
initStrm Stream { streamId       = id
                , streamBuffer   = buffer
                , streamExprType = t } =
  (id, toDyn (reverse buffer))

-- | Evaluate several streams for a number of steps, producing the environment
-- at the end of the evaluation.
evalStreams :: Int -> [Stream] -> Env Id -> Env Id
evalStreams top specStrms initStrms =
  -- XXX actually only need to compute until shortest stream is of length k
  -- XXX this should just be a foldl' over [0,1..k]
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
      let xs = fromJust $ lookup id strms >>= fromDynamic      in
      let x  = evalExpr_ k e [] strms                          in
      let ls = x `seq` (x:xs)                                  in
      (id, toDyn ls)

--------------------------------------------------------------------------------

-- | Evaluate a trigger for a number of steps.
evalTrigger :: ShowType          -- ^ Show booleans as @0@/@1@ (C) or
                                 --   @True@/@False@ (Haskell).
            -> Int               -- ^ Number of steps to evaluate.
            -> Env Id            -- ^ Environment to use with known
                                 --   stream-value associations.
            -> Trigger           -- ^ Trigger to evaluate.
            -> [Maybe [Output]]
evalTrigger showType k strms
  Trigger
    { triggerGuard = e
    , triggerArgs  = args
    } = map tag (zip bs vs)

  where
  tag :: (Bool, a) -> Maybe a
  tag (True,  x) = Just x
  tag (False, _) = Nothing

  -- Is the guard true?
  bs :: [Bool]
  bs = evalExprs_ k e strms

  -- The argument outputs.
  vs :: [[Output]]
  vs = if null args then replicate k []  -- might be 0 args.
         else transpose $ map evalUExpr args

  evalUExpr :: UExpr -> [Output]
  evalUExpr (UExpr t e1) =
    map (showWithType showType t) (evalExprs_ k e1 strms)

--------------------------------------------------------------------------------

-- | Evaluate an observer for a number of steps.
evalObserver :: ShowType  -- ^ Show booleans as @0@/@1@ (C) or @True@/@False@
                          --   (Haskell).
             -> Int       -- ^ Number of steps to evaluate.
             -> Env Id    -- ^ Environment to use with known stream-value
                          --   associations.
             -> Observer  -- ^ Observer to evaluate.
             -> [Output]
evalObserver showType k strms
  Observer
    { observerExpr     = e
    , observerExprType = t }
  = map (showWithType showType t) (evalExprs_ k e strms)

--------------------------------------------------------------------------------

-- | Evaluate an expression for a number of steps, producing a list with the
-- changing value of the expression until that time.
evalExprs_ :: Typeable a => Int -> Expr a -> Env Id -> [a]
evalExprs_ k e strms =
  map (\i -> evalExpr_ i e [] strms) [0..(k-1)]

--------------------------------------------------------------------------------

-- | Safe indexing (!!) on possibly infininite lists.
safeIndex :: Int -> [a] -> Maybe a
safeIndex i ls =
  let ls' = take (i+1) ls in
  if length ls' > i then Just (ls' !! i)
    else Nothing

-------------------------------------------------------------------------------
