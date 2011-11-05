{- Copyright (c) 2011 National Institute of Aerospace / Galois, Inc. -}

{-# LANGUAGE GADTs, FlexibleInstances #-}

-- | The interpreter.

module Copilot.Language.Interpret
  ( Input
  , csv
  , interpret 
  , var
  , array
  ) where

import Copilot.Core.Type (Typed, typeOf)
import Copilot.Core.Interpret (ExtEnv (..))
import Copilot.Core.Type.Dynamic (toDynF)
import qualified Copilot.Core.Interpret as I
import Copilot.Language.Spec (Spec)
import Copilot.Language.Reify

import Data.List (foldl')

data Input where
  Var :: Typed a => String -> [a] -> Input
  Arr :: Typed a => String -> [[a]] -> Input

var :: Typed a => String -> [a] -> Input
var = Var

array :: Typed a => String -> [[a]] -> Input
array = Arr

csv :: Integer -> [Input] -> Spec -> IO ()
csv i input_ spec = do
  putStrLn "Note: CSV format does not output observers."
  interpret' I.CSV i input_ spec

-- | Much slower, but pretty-printed interpreter output.  
interpret :: Integer -> [Input] -> Spec -> IO ()
interpret = interpret' I.Table

interpret' :: I.Format -> Integer -> [Input] -> Spec -> IO ()
interpret' format i inputs spec = do
  coreSpec <- reify spec
  putStrLn $ I.interpret format (fromIntegral i) exts coreSpec
  where
  exts = foldl' env (ExtEnv [] []) inputs
  env acc (Var name xs) = 
    acc { varEnv = (name, toDynF typeOf xs) : varEnv acc } 
  env acc (Arr name xs) = 
    acc { arrEnv = (name, map (toDynF typeOf) xs) : arrEnv acc }


