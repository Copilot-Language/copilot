{-# LANGUAGE TypeFamilies #-}

{-
 - Basic file containing a main function, used for testing the compilation
 -}

module Main where

import Language.Copilot
import Copilot.Core.Type

import Copilot.Compile.C
import qualified Prelude as P

data Vec = Vec  { v1 :: Int8
                , v2 :: Int16
                } deriving Show

instance Typed Vec where
  typeOf = Struct (Vec 0 0)

instance ArrayItem Vec

instance Struct Vec where
  typename _  = TyTypedef "vec"
  toValues v  = [ V Int8  "v1" (v1 v)
                , V Int16 "v2" (v2 v)
                ]
  fromValues  ( V Int8  "v1" x
              : V Int16 "v2" y
              : []
              ) = Vec x y

-------------------------------------------------------------------------------

s :: Stream Vec
s = [Vec 1 2, Vec 3 4] ++ exvec

s' :: Stream Vec
s' = [Vec 5 6] ++ s


exvec :: Stream Vec
exvec = extern "exvec" Nothing

spec :: Spec
spec = do
  trigger "fstruct" true [arg s]

main :: IO ()
main = do reify spec >>= compile




vec' =  [ V Int8  "v2" 10
        , V Int16 "v1" 12
        ]
