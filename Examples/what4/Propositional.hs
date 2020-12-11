module Main where

import Language.Copilot
import Copilot.Theorem.What4
import qualified Prelude as P
import Control.Monad (void, forM_)

spec :: Spec
spec = do

  -- The constant value true, which is translated as the corresponding SMT
  -- boolean literal.
  void $ prop "Example 1" (forall true)

  -- The constant value false, which is translated as the corresponding SMT
  -- boolean literal.
  void $ prop "Example 2" (forall false)

  -- An inductively defined flavor of true, which requires induction to prove,
  -- and hence is found to be invalid by the SMT solver (since no inductive
  -- hypothesis is made).
  let a = [True] ++ a
  void $ prop "Example 3" (forall a)

  -- An inductively defined "a or not a" proposition, which is unprovable by
  -- the SMT solver.
  let a = [False] ++ b
      b = [True] ++ a
  void $ prop "Example 4" (forall (a || b))

  -- A version of "a or not a" proposition which does not require any sort of
  -- inductive argument, and hence is provable.
  let a = [False] ++ b
      b = not a
  void $ prop "Example 5" (forall (a || b))

  -- A bit more convoluted version of Example 5, which is provable.
  let a = [True, False] ++ b
      b = [False] ++ not (drop 1 a)
  void $ prop "Example 6" (forall (a || b))

  -- An example using external streams.
  let a = extern "a" Nothing
  void $ prop "Example 7" (forall (a || not a))

main :: IO ()
main = do
  spec' <- reify spec
  results <- prove Z3 spec'

  forM_ results $ \(nm, res) -> do
    putStr $ nm <> ": "
    case res of
      Valid -> putStrLn "valid"
      Invalid -> putStrLn "invalid"
      Unknown -> putStrLn "unknown"

  interpret 10 spec
