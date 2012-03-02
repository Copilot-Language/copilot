{- Copyright (c) 2011 National Institute of Aerospace / Galois, Inc. -}

-- | The interpreter.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs, FlexibleInstances #-}

module Copilot.Language.Interpret
  ( --Input
    csv
  , interpret 
--  , var
--  , array
--  , func
  ) where

--import Copilot.Core.Type (Typed, typeOf)
--import Copilot.Core.Interpret (ExtEnv (..))
--import Copilot.Core.Type.Dynamic (toDynF)
import qualified Copilot.Core.Interpret as I

import Copilot.Language.Spec (Spec)
import Copilot.Language.Reify

--import Data.List (foldl')

--------------------------------------------------------------------------------

-- data Input where
--   -- External variables.
--   Var  :: Typed a => String -> [a] -> Input
--   -- External arrays (list of lists).
--   Arr  :: Typed a => String -> [[a]] -> Input
--   -- -- External functions (streams).
--   -- Func :: Typed a => String -> Stream a -> Input

-- var :: Typed a => String -> [a] -> Input
-- var = Var

-- array :: Typed a => String -> [[a]] -> Input
-- array = Arr

-- func :: Typed a => String -> Stream a -> Input
-- func = Func

--------------------------------------------------------------------------------

csv :: Integer -> Spec -> IO ()
csv i spec = do
  putStrLn "Note: CSV format does not output observers."
  interpret' I.CSV i spec

--------------------------------------------------------------------------------

-- | Much slower, but pretty-printed interpreter output.  
interpret :: Integer -> Spec -> IO ()
interpret = interpret' I.Table

interpret' :: I.Format -> Integer -> Spec -> IO ()
interpret' format i spec = do
  coreSpec <- reify spec
--  fexts    <- funcExts
  putStrLn $ I.interpret format (fromIntegral i) coreSpec

--  where
--   unionExts :: ExtEnv
--   unionExts = ExtEnv { varEnv  = varEnv varArrExts
--                      , arrEnv  = arrEnv varArrExts
-- --                     , funcEnv = fexts
--                      }

  -- -- We do the two folds below over the data type separately, since one
  -- -- component is monadic.
  -- funcExts :: IO [(Name, C.Spec)]
  -- funcExts = 
  --   let (names, specs) = unzip $ foldl' envf [] inputs in
  --   do ss <- sequence specs
  --      return $ zip names ss
  --   where
  --   envf :: [(Name, IO C.Spec)] -> Input -> [(Name, IO C.Spec)]
  --   envf acc (Func name strm) = 
  --     (name, reify $ observer name strm) : acc
  --   envf acc _ = acc

  -- varArrExts :: ExtEnv
  -- varArrExts = foldl' env (ExtEnv [] []) inputs
  --   where 
  --   env :: ExtEnv -> Input -> ExtEnv
  --   env acc (Var name xs) = 
  --     acc { varEnv = (name, toDynF typeOf xs) : varEnv acc } 
  --   env acc (Arr name xs) = 
  --     acc { arrEnv = (name, map (toDynF typeOf) xs) : arrEnv acc }
--    env acc _ = acc

--------------------------------------------------------------------------------
