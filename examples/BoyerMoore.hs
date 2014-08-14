--------------------------------------------------------------------------------

{-# LANGUAGE RebindableSyntax, ScopedTypeVariables #-}

module BoyerMoore (spec, scheme) where

import Prelude ()
import Control.Monad (forM_)

import Language.Copilot hiding (length)
import Copilot.Kind
import Copilot.Kind.Lib (arbitraryCstW8)

import qualified Prelude   as P
import qualified Data.List as L

--------------------------------------------------------------------------------

length :: [a] -> Stream Word8
length l = constant (fromInteger $ L.genericLength l)


--------------------------------------------------------------------------------
majorityVote :: forall a . (Typed a, Eq a) => [Stream a] -> Stream a
majorityVote [] = error "empty list"
majorityVote (x : xs) = aux x 1 xs
  where
  aux :: Stream a -> Stream Word8 -> [Stream a] -> Stream a
  aux p _s [] = p
  aux p s (l : ls) =
    local (if s == 0 then l else p) $ \p' ->
    local (if s == 0 || l == p then s + 1 else s - 1) $ \s' ->
    aux p' s' ls


okWith :: 
  forall a . (Typed a, Eq a) => 
  Stream a -> [Stream a] -> Stream a -> Stream Bool
  
okWith a l maj = (a /= maj) ==> ((2 * count a l) <= length l)
  where
  count :: Stream a -> [Stream a] -> Stream Word8
  count _e [] = 0
  count e (x : xs) = (if x == e then 1 else 0) + count e xs

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  forM_ (zip [1..] ss) $ \(k :: Int, s) ->
    observer ((P.++) "s" (show k)) s
  observer "maj" maj
  
  prop "OK" (okWith (arbitraryCstW8 "n") ss maj)
  prop "i1" (s1 == 1 && s2 == 1 && s3 == 1 && s4 == 1)
  prop "r1" (maj == 1)

  where
  
    s1 :: Stream Word8
    s1 = externW8 "s1" (Just $ repeat 1)
    s2 = externW8 "s2" (Just $ repeat 3)
    s3 = externW8 "s3" (Just $ repeat 1)
    s4 = externW8 "s4" (Just $ repeat 1)
    s5 = externW8 "s5" (Just $ repeat 2)
    s6 = externW8 "s6" (Just $ repeat 2)
    s7 = externW8 "s7" (Just $ repeat 1)
    ss = [s1, s2, s3, s4, s5, s6, s7]
    
    maj = majorityVote ss

--------------------------------------------------------------------------------

scheme :: ProofScheme
scheme = do
  check "OK"
  assuming ["i1"] $ check "r1"

--------------------------------------------------------------------------------
