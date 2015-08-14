--------------------------------------------------------------------------------

{-# LANGUAGE RebindableSyntax, ScopedTypeVariables #-}

module BoyerMoore where

import Prelude ()
import Control.Monad (forM_)

import Copilot.Language hiding (length)
import Copilot.Theorem
import Copilot.Theorem.Prover.Z3

import Copilot.Core.Type
import Copilot.Core.Type.Uninitialized

import qualified Prelude   as P
import qualified Data.List as L

--------------------------------------------------------------------------------

length :: [a] -> Stream Word8
length l = constant (fromInteger $ L.genericLength l)

arbitraryCst :: forall a . (Typed a) => String -> Stream a
arbitraryCst s = c
  where
    t :: Stream Word8
    t = [0] ++ (1 + t)
    i :: Stream a
    i = extern s Nothing
    c = if t == 0 then i else [uninitialized (typeOf :: Type a)] ++ c

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

spec = do
  forM_ (zip [1..] ss) $ \(k :: Int, s) ->
    observer ((P.++) "s" (show k)) s
  observer "maj" maj

  i1 <- prop "i1" (forall $ s1 == 1 && s2 == 1 && s3 == 1 && s4 == 1)
  theorem "r1" (forall $ maj == 1) $ assume i1 >> induct
  theorem "OK" (forall $ okWith (arbitraryCst "n") ss maj) induct

  where
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

induct :: Proof Universal
induct = induction def { nraNLSat = False, debug = False }

