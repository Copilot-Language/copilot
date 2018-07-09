{-# LANGUAGE DataKinds #-}

module Main where

import Language.Copilot
import Copilot.Compile.C
import qualified Prelude as P

temp :: Stream Int8
temp = extern "temp" Nothing

counter :: Stream Int8
counter = [1] ++ (counter + 1)

fib :: Stream Int32
fib = [1, 1] ++ (fib + drop 1 fib)

spec = do
  trigger "alarm" (temp > 65) [arg counter]
  trigger "fib" (fib > 1000) []

main = do reify spec >>= compile (defaultParams { prefix = Just "reportexample" })
