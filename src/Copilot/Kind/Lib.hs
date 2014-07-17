--------------------------------------------------------------------------------

{-# LANGUAGE RebindableSyntax #-}

module Copilot.Kind.Lib
  ( noInlining
  , conj
  , disj
  , forAllCst
  , existsCst
  , arbitraryCstW8
  ) where


import Prelude ()
import Copilot.Language

--------------------------------------------------------------------------------

noInlining :: (Typed a) => Stream a -> Stream a
noInlining = flip local id

conj :: [Stream Bool] -> Stream Bool
conj cs = foldl (&&) true cs

disj :: [Stream Bool] -> Stream Bool
disj cs = foldl (||) false cs

forAllCst :: [Word8] -> (Stream Word8 -> Stream Bool) -> Stream Bool
forAllCst l f = conj $ map (f . constant) l

existsCst :: [Word8] -> (Stream Word8 -> Stream Bool) -> Stream Bool
existsCst l f = disj $ map (f . constant) l

-- A dirty hack to build a constant stream of unknown value
arbitraryCstW8 :: String -> Stream Word8
arbitraryCstW8 s = a
  where t = [0] ++ (1 + t)
        i = externW8 s Nothing
        a = if t == 0 then i else [0] ++ t
        
--------------------------------------------------------------------------------
        