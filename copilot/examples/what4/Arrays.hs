-- | An example showing the usage of the What4 backend in copilot-theorem for
-- arrays where individual elements are updated.

{-# LANGUAGE DataKinds #-}

module Main where

import qualified Prelude as P
import Control.Monad (void, forM_)

import Language.Copilot
import Copilot.Theorem.What4

spec :: Spec
spec = do
  let pair :: Stream (Array 2 Int16)
      pair = extern "pair" Nothing

  -- Check equality, indexing into array and modifying the value. Note that
  -- this is trivial by equality.
  void $ prop "Example 1" $ forAll $
    ((pair !! 0 =$ (+1)) ! 0) == ((pair ! 0) + 1)

  -- Same as previous example, but get a different array index (so should be
  -- false).
  void $ prop "Example 2" $ forAll $
    ((pair !! 0 =$ (+1)) ! 1) == ((pair ! 0) + 1)

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
