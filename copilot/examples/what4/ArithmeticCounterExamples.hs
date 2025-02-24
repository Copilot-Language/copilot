-- | An example showing the usage of the What4 backend in copilot-theorem for
-- simple arithmetic. This example uses the 'proveWithCounterExamples' function
-- to demonstrate counterexamples in the event of invalid properties.

module Main where

import qualified Prelude as P
import Control.Monad (void, forM_)
import qualified Data.Map as Map

import Language.Copilot
import Copilot.Theorem.What4

spec :: Spec
spec = do
  -- Define some external streams. Their values are not important, so external
  -- streams suffice.
  let eint8  :: Stream Int8
      eint8  = extern "eint8" Nothing
      eword8 :: Stream Word8
      eword8 = extern "eword8" Nothing
      efloat :: Stream Float
      efloat = extern "efloat" Nothing

  -- The simplest example involving numbers: equality on constant values.
  void $ prop "Example 1" (forAll ((constant (1 :: Int8)) == (constant 1)))

  -- Testing "a < a + 1". This should fail, because it isn't true.
  void $ prop "Example 2" (forAll (eint8 < (eint8 + 1)))

  -- Adding another condition to the above property to make it true.
  void $ prop "Example 3" (forAll ((eint8 < (eint8 + 1)) || (eint8 == 127)))

  -- Just like the previous example, but with words.
  void $ prop "Example 4" (forAll ((eword8 < (eword8 + 1)) || (eword8 == 255)))

  -- An example with floats.
  void $ prop "Example 5" (forAll ((2 * efloat) == (efloat + efloat)))

  -- Another example with floats. This fails, because it isn't true.
  void $ prop "Example 6" (forAll ((efloat + 1) /= efloat))

main :: IO ()
main = do
  spec' <- reify spec

  -- Use Z3 to prove the properties.
  results <- proveWithCounterExample Z3 spec'

  -- Print the results.
  forM_ results $ \(nm, res) -> do
    putStr $ nm <> ": "
    case res of
      ValidCex -> putStrLn "valid"
      InvalidCex cex -> do
        putStrLn "invalid"
        putStrLn $ ppCounterExample cex
      UnknownCex -> putStrLn "unknown"

-- | Pretty-print a counterexample for user display.
ppCounterExample :: CounterExample -> String
ppCounterExample cex
    | any P.not (baseCases cex)
    = if Map.null baseCaseVals
        then
          "  All possible extern values during the base case(s) " P.++
          "constitute a counterexample."
        else
          unlines $
            "  The base cases failed with the following extern values:" :
            map
              (\((name, _), val) -> "    " P.++ name P.++ ": " P.++ show val)
              (Map.toList baseCaseVals)

    | P.not (inductionStep cex)
    = if Map.null inductionStepVals
        then
          "  All possible extern values during the induction step " P.++
          "constitute a counterexample."
        else
          unlines $
            "  The induction step failed with the following extern values:" :
            map
              (\((name, _), val) -> "    " P.++ name P.++ ": " P.++ show val)
              (Map.toList inductionStepVals)

    | otherwise
    = error $
        "ppCounterExample: " P.++
        "Counterexample without failing base cases or induction step"
  where
    allExternVals = concreteExternValues cex

    baseCaseVals =
      Map.filterWithKey
        (\(_, offset) _ ->
          case offset of
            AbsoluteOffset {} -> True
            RelativeOffset {} -> False
        )
        allExternVals

    inductionStepVals =
      Map.filterWithKey
        (\(_, offset) _ ->
          case offset of
            AbsoluteOffset {} -> False
            RelativeOffset {} -> True
        )
        allExternVals
