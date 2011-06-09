--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

-- |

module Copilot.Language.Interpret
  ( Input
  , interpret
  , input
  ) where

import Copilot.Core.Type (Typed, typeOf)
import Copilot.Core.Type.Dynamic (toDynamicF)
import qualified Copilot.Core.Interpret as I
import Copilot.Language.Stream (Trigger)
import Copilot.Language.Reify

data Input where
  Input :: Typed a => String -> [a] -> Input

input :: Typed a => String -> [a] -> Input
input = Input

interpret
  :: Integer
  -> [Input]
  -> [Trigger]
  -> IO ()
interpret i inputs triggers =
  do
    spec <- reify triggers
    putStrLn $ I.interpret (fromIntegral i) exts spec
  where
    exts = map (\ (Input name xs) -> (name, toDynamicF xs typeOf)) inputs