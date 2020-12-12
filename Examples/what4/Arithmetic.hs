-- | An example showing the usage of the What4 backend in copilot-theorem for
-- simple arithmetic.

module Main where

import Language.Copilot
import Copilot.Theorem.What4
import qualified Prelude as P
import Control.Monad (void, forM_)

spec :: Spec
spec = do
  -- Define some external streams. Their values are not important, so external
  -- streams suffice.
  let eint8 :: Stream Int8
      eint8 = extern "eint8" Nothing
      eword8 :: Stream Word8
      eword8 = extern "eword8" Nothing
      efloat :: Stream Float
      efloat = extern "efloat" Nothing

  -- The simplest example involving numbers: equality on constant values.
  void $ prop "Example 1" (forall ((constant (1 :: Int8)) == (constant 1)))

  -- Testing "a < a + 1". This should fail, because it isn't true.
  void $ prop "Example 2" (forall (eint8 < (eint8 + 1)))

  -- Adding another condition to the above property to make it true.
  void $ prop "Example 3" (forall ((eint8 < (eint8 + 1)) || (eint8 == 127)))

  -- Just like the previous example, but with words.
  void $ prop "Example 4" (forall ((eword8 < (eword8 + 1)) || (eword8 == 255)))

  -- An example with floats.
  void $ prop "Example 5" (forall ((2 * efloat) == (efloat + efloat)))

  -- Another example with floats. This fails, because it isn't true.
  void $ prop "Example 6" (forall ((efloat + 1) /= efloat))

main :: IO ()
main = do
  spec' <- reify spec

  -- Use Z3 to prove the properties.
  results <- prove Z3 spec'

  -- Print the results.
  forM_ results $ \(nm, res) -> do
    putStr $ nm <> ": "
    case res of
      Valid -> putStrLn "valid"
      Invalid -> putStrLn "invalid"
      Unknown -> putStrLn "unknown"

  interpret 10 spec
