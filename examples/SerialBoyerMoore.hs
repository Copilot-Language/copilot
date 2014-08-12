--------------------------------------------------------------------------------
{-# LANGUAGE RebindableSyntax #-}

module SerialBoyerMoore (spec, scheme) where

import Prelude ()
import Language.Copilot hiding (majority)
import Copilot.Kind
import Copilot.Kind.Lib (forAllCst, existsCst)

--------------------------------------------------------------------------------

allowed :: [Word8]
allowed = [1, 2]

majority :: Stream Word8 -> (Stream Word8, Stream Word8, Stream Bool)
majority l = (p, s, j)
  where
    p  = [0] ++ if s <= 0 then l else p
    s  = [0] ++ if p == l || s <= 0 then s + 1 else s - 1
    
    k  = [0] ++ (1 + k)
    
    count m = cnt
      where cnt = [0] ++ if l == m then cnt + 1 else cnt
    
    j = forAllCst allowed $ \m ->
          local (count m) $ \cnt ->
          let j0 = (m /= p) ==> ((s + 2 * cnt) <= k)
              j1 = (m == p) ==> ((2 * cnt) <= (s + k))
          in j0 && j1

spec = do
  observer "i" input
  observer "p" p
  observer "s" s
  observer "j" j

  prop "J"  j
  prop "inRange" (existsCst allowed $ \a -> input == a)
  
  where
    input = externW8 "in" (Just [1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 2, 1])
    (p, s, j) = majority input
    

scheme = do
  assuming ["inRange"] $ check "J"
  
--------------------------------------------------------------------------------
