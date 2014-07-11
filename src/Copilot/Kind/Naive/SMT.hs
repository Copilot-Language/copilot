--------------------------------------------------------------------------------

module Copilot.Kind.Naive.SMT
  ( Solver
  , startNewSolver
  , assume
  , entailed
  , exit
  , SatResult (..)
  ) where

import Copilot.Kind.IL
import Copilot.Kind.Misc.SExpr

import System.IO
import System.Process
import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe 

--------------------------------------------------------------------------------

data Solver = Solver 
  { inh        :: Handle
  , outh       :: Handle
  , process    :: ProcessHandle
  , debugMode  :: Bool
  , solverName :: String }

 
type SSExpr = SExpr String

data SatResult 
  = Sat SSExpr
  | Unsat
  | Unknown

--------------------------------------------------------------------------------

varN :: String
varN = "n"

smtTy :: Type t -> String
smtTy Integer = "Int"
smtTy Bool    = "Bool"

debug :: Bool -> Solver -> String -> IO ()
debug printName s str = when (debugMode s) $ do
  putStrLn $ (if printName then "<" ++ solverName s ++ ">  " else "") ++ str

send :: Solver -> SSExpr -> IO ()
send s t = do
  hPutStr (inh s) (show t ++ "\n")
  debug True s (show t)
  hFlush (inh s)

receive :: Solver -> IO String
receive s = do
  answer <- hGetLine (outh s)
  debug False s answer
  return answer
    
startNewSolver :: String -> [SeqDescr] -> Bool -> IO Solver
startNewSolver name unint dbgMode = do
  (i, o, e, p) <- runInteractiveProcess cmd opts Nothing Nothing
  hClose e
  let s = Solver i o p dbgMode name
  send s (_setLogic logic)
  send s (_declConst Integer varN)
  forM_ unint $ \(SeqDescr id ty) ->
    send s (_declSeq ty id)
  return s

  where cmd  = "yices-smt2"
        opts = ["--incremental"]
        logic = "QF_UFLIA"
  

exit :: Solver -> IO ()
exit s = do
  hClose (inh s)
  hClose (outh s)
  terminateProcess (process s)

assume :: Solver -> [Constraint] -> IO ()
assume s cs = forM_ cs assumeOne
  where assumeOne c = send s (_assert c)
  

entailed :: Solver -> [Constraint] -> IO SatResult
entailed s cs = do
  send s _push
  assume s $
    [foldl (Op2 Bool Or) (Const Bool False) (map (Op1 Bool Not) cs)]
  send s _checkSat

  res <- receive s >>= \case
    "sat"     -> return $ Sat unit
    "unsat"   -> return Unsat
    "unknown" -> return Unknown
    _         -> error ""

  send s _pop
  return res

-- Not supported by Yices 2
queryAssignment :: Solver -> IO SSExpr
queryAssignment s = do
  send s (node "get-value" [list [atom "s0", atom "57"]])
  answer <- fromJust . parseSExpr <$> hGetContents (outh s)
  --answer <- receive s
  print answer
  return unit
    
--------------------------------------------------------------------------------

_declConst :: Type a -> String -> SSExpr
_declConst t id = node "declare-fun" [atom id, unit, atom $ smtTy t]

_declSeq :: Type a -> String -> SSExpr
_declSeq t id = node "declare-fun"
                [atom id, singleton (smtTy Integer), atom $ smtTy t]

_assert :: Constraint -> SSExpr
_assert c = node "assert" [_expr c]

_push :: SSExpr
_push = node "push" [atom "1"]

_pop :: SSExpr
_pop = node "pop" [atom "1"]

_checkSat :: SSExpr
_checkSat = singleton "check-sat"

_setLogic :: String -> SSExpr
_setLogic l = node "set-logic" [atom l]

--------------------------------------------------------------------------------

_expr :: Expr t -> SSExpr

_expr (Const Integer v) = atom (show v)
_expr (Const Bool    b) = atom (if b then "true" else "false")

_expr (Ite _ cond e1 e2) = node ("ite")
                             [_expr cond, _expr e1, _expr e2]

_expr (Op1 _ op e) =
  node smtOp [_expr e]
  where
    smtOp = case op of
      Not -> "not"
      Neg -> "-"

_expr (Op2 _ op e1 e2) =
  node smtOp [_expr e1, _expr e2]
  where
    smtOp = case op of
      EqB  -> "="
      EqI  -> "="
      Le   -> "<="
      Lt   -> "<"
      Ge   -> ">="
      Gt   -> ">"
      And  -> "and"
      Or   -> "or"
      Add  -> "+"
      Sub  -> "-"
      Mul  -> "*"

_expr (SVal _ f ix) = node f [_ix]

  where _ix = case ix of
          Fixed i -> atom (show i)
          Var off -> node "+"
                     [atom varN, atom (show off)]
                     
--------------------------------------------------------------------------------
