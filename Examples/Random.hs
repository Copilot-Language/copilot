--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Generate a random spec and pretty-print it.

module Random ( randomEx ) where

import Copilot.Core.PrettyPrint as P
import Copilot.Core.Random (randomSpec)
import Copilot.Core.Random.Weights (simpleWeights)
import System.Random (newStdGen)

randomEx :: IO ()
randomEx = do
  g <- newStdGen
  let p = randomSpec 10 simpleWeights g -- have to give a dummy number of rounds
                                        -- to simulate to generate a
                                        -- spec---mostly used for
                                        -- interpreting/QuickCheck testing.
  putStrLn (P.prettyPrint p)
