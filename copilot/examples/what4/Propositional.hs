-- | An example showing the usage of the What4 backend in copilot-theorem for
-- propositional logic on boolean streams.

module Main where

import qualified Prelude as P
import Control.Monad (void, forM_)

import Language.Copilot
import Copilot.Theorem.What4

spec :: Spec
spec = do
  -- * Non-inductive propositions

  -- The constant value true, which is translated as the corresponding SMT
  -- boolean literal (and is therefore provable).
  void $ prop "Example 1" (forAll true)

  -- The constant value false, which is translated as the corresponding SMT
  -- boolean literal (and is therefore not provable).
  void $ prop "Example 2" (forAll false)

  -- An "a or not a" proposition which does not require any sort of inductive
  -- argument (but see examples 5 and 6 below for versions that do require
  -- induction to solve). This is easily proven.
  let a = [False] ++ b
      b = not a
  void $ prop "Example 3" (forAll (a || b))

  -- An "a or not a" proposition using external streams, which is also provable.
  let a = extern "a" Nothing
  void $ prop "Example 4" (forAll (a || not a))

  -- * Simple inductive propositions
  --
  -- While Copilot.Theorem.What4 is not able to solve all inductive propositions
  -- in general (see the "Complex inductive propositions" section below), the
  -- following inductive propositions are simple enough that the heuristics in
  -- Copilot.Theorem.What4 can solve them without issue.

  -- An inductively defined flavor of true.
  let a = [True] ++ a
  void $ prop "Example 5" (forAll a)

  -- An inductively defined "a or not a" proposition (i.e., a more complex
  -- version of example 3 above).
  let a = [False] ++ b
      b = [True] ++ a
  void $ prop "Example 6" (forAll (a || b))

  -- A bit more convoluted version of example 6.
  let a = [True, False] ++ b
      b = [False] ++ not (drop 1 a)
  void $ prop "Example 7" (forAll (a || b))

  -- * Complex induction propositions
  --
  -- The heuristics in Copilot.Theorem.What4 are not able to prove these
  -- inductive propositions, so these will be reported as unprovable, even
  -- though each proposition is actually provable.

  -- An inductively defined flavor of true (i.e., a more complex version of
  -- example 5 above).
  let a = [True] ++ ([True] ++ ([True] ++ a))
  void $ prop "Example 8" (forAll a)

  -- An inductively defined "a or not a" proposition (i.e., a more complex
  -- version of example 6 above).
  let a = [False] ++ ([False] ++ ([False] ++ b))
      b = [True] ++ ([True] ++ ([True] ++ a))
  void $ prop "Example 9" (forAll (a || b))

main :: IO ()
main = do
  spec' <- reify spec

  -- Use Z3 to prove the properties.
  results <- prove Z3 spec'

  -- Print the results.
  forM_ results $ \(nm, res) -> do
    putStr $ nm <> ": "
    case res of
      Valid   -> putStrLn "valid"
      Invalid -> putStrLn "invalid"
      Unknown -> putStrLn "unknown"
