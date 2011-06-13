--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Main where

import qualified Prelude as P
import Copilot.Language
import Copilot.Language.Prelude

--------------------------------------------------------------------------------

addMult :: Spec
addMult =
  do
    let_ "v" summation
    trigger "f" true [ arg $ mult 5 ]

  where

  summation = nats + nats + nats

  mult :: Int -> Stream Word64
  mult 0 = var "v"
  mult i = var "v" * mult (i-1)

  nats :: Stream Word64
  nats = [0] ++ nats + 1

main :: IO ()
main =
  do
    putStrLn "PrettyPrinter:"
    putStrLn ""
    prettyPrint addMult
    putStrLn ""
    putStrLn ""
    putStrLn "Interpreter:"
    putStrLn ""
    interpret 100 [] addMult


--------------------------------------------------------------------------------