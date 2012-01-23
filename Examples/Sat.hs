-- | Example of a simple SAT solver (simply constructs the truth table) in
-- Copilot.

-- {-# LANGUAGE RebindableSyntax #-}

module Languages where

import Language.Copilot
import qualified Prelude as P
import Control.Monad (foldM_)

---------------------------------------------------------------------------------

-- | Number of sat variables and a Boolean function over those variables.
type SatFunc = (Int, [Stream Bool] -> Stream Bool)

-- Takes a Sat function and returns the stream of variables and a SAT function.
sat :: SatFunc -> ([Stream Bool], Stream Bool)
sat (i,f) = (xs, res)
  where
  -- If it becomes true, it stays true.
  res   = [False] ++ (f xs || res)

  xs  = xs' i

  xs' 0 = []
  xs' n = clk (period $ 2 P.^ n) (phase (0 :: Int)) : xs' (n P.- 1)

satSpec :: SatFunc -> Spec
satSpec f = do
  observer "sat" (snd $ sat f)
  observer "done" done
  foldM_ mkObs (0 :: Int) xs

  where
  xs = fst $ sat f
  cnt :: Stream Word64 -- Would need to be extended depending on number of
                       -- variables.
  cnt = [1] ++ cnt + 1
  done = cnt == (2 P.^ length xs)
  mkObs idx strm = do observer ("obs" P.++ show idx) strm
                      return (idx P.+ 1)

-- Example SAT function.
f0 :: SatFunc
f0 = (3, \[a,b,c] -> a && (b || (not c)))

f1 :: SatFunc
f1 = (9, \[a,b,c,d,e,f,g,h,i] -> a && b && c && d && e && 
                                 f && g && h && i && i && not i)

-- | Run the interpreter long enough to get an answer.
runFunc :: SatFunc -> IO ()
runFunc f = interpret (2 P.^ fst f) (satSpec f)

---------------------------------------------------------------------------------
