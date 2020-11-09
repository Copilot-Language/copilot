{-# LANGUAGE DataKinds #-}

module Main where

import Language.Copilot
import Copilot.Theorem.What4
import qualified Prelude as P
import Control.Monad (void, forM_)

data Volts = Volts { numVolts :: Field "numVolts" Word16
                   , flag     :: Field "flag" Bool
                   }

instance Struct Volts where
  typename _ = "volts"
  toValues volts = [ Value Word16 (numVolts volts)
                   , Value Bool (flag volts)
                   ]

instance Typed Volts where
  typeOf = Struct (Volts (Field 0) (Field False))

data Battery = Battery { temp  :: Field "temp" Word16
                       , volts :: Field "volts" (Array 10 Volts)
                       , other :: Field "other" (Array 10 (Array 5 Word32))
                       }

instance Struct Battery where
  typename _ = "battery"
  toValues battery = [ Value typeOf (temp battery)
                     , Value typeOf (volts battery)
                     , Value typeOf (other battery)
                     ]

instance Typed Battery where
  typeOf = Struct (Battery (Field 0) (Field undefined) (Field undefined))

spec :: Spec
spec = do
  let battery :: Stream Battery
      battery = extern "battery" Nothing

  -- Check equality, indexing into nested structs and arrays.
  void $ prop "Example 1" $ forall $
    (((battery # volts) .!! 0) # numVolts) == (((battery # volts) .!! 0) # numVolts)

  -- Same as previous example, but get a different array index (so should be false).
  void $ prop "Example 2" $ forall $
    (((battery # other) .!! 2) .!! 3) == (((battery # other) .!! 2) .!! 4)

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
