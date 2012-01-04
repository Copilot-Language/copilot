--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Another small example.

module AddMult ( addMult ) where

import Prelude ()
import Language.Copilot

--------------------------------------------------------------------------------

spec :: Spec
spec = 
  trigger "f" true [ arg $ mult 5 ]

  where
  mult :: Word64 -> Stream Word64
  mult 0 = 1
  mult i = constant i * mult (i-1)

addMult :: IO ()
addMult = do
  putStrLn "PrettyPrinter:"
  putStrLn ""
  prettyPrint spec
  putStrLn ""
  putStrLn ""
  putStrLn "Interpreter:"
  putStrLn ""
  interpret 100 spec


--------------------------------------------------------------------------------
