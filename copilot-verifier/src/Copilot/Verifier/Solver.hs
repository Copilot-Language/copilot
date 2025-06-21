module Copilot.Verifier.Solver
  ( Solver(..)
  , solverAdapter
  ) where

import qualified What4.Solver.Adapter as W4
import qualified What4.Solver.CVC4 as W4.CVC4
import qualified What4.Solver.CVC5 as W4.CVC5
import qualified What4.Solver.Yices as W4.Yices
import qualified What4.Solver.Z3 as W4.Z3

-- | General-purpose SMT solvers that @copilot-verifier@ supports.
data Solver
  = CVC4
  | CVC5
  | Yices
  | Z3
  deriving Show

-- | Return the @what4@ 'W4.SolverAdapter' corresponding to a given 'Solver'.
solverAdapter :: Solver -> W4.SolverAdapter st
solverAdapter CVC4  = W4.CVC4.cvc4Adapter
solverAdapter CVC5  = W4.CVC5.cvc5Adapter
solverAdapter Yices = W4.Yices.yicesAdapter
solverAdapter Z3    = W4.Z3.z3Adapter
