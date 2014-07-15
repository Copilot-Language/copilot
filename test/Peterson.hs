{-# LANGUAGE RebindableSyntax #-}

module Peterson (spec, scheme) where

import Prelude ()
import Copilot.Language
import Copilot.Kind.ProofScheme


data Process = P1 | P2 deriving (Eq, Ord)


petersen :: (Process -> Stream Bool) -> (Stream Bool, Process -> Stream Bool)
petersen want = (turn, critic)

  where
    turn = undefined  
    critic P1 = undefined
    critic P2 = undefined
        

idle :: Word8
idle = 0

spec :: Spec
spec = do
  --prop "stayIdle" (st == constant idle)
  prop "vneq0" (z /= 0)
  prop "rpos" (r >= 0)
  observer "obs" (z /= 0)
  observer "obs2" (local (st) (\x -> x - x))
  where st :: Stream Word8
        st = [idle] ++ st + drop 0 indirect
        
        z :: Stream Word8
        z = local (st) (\x -> x * x)
        
        indirect :: Stream Word8
        indirect = v + 1
        
        r :: Stream Float
        r = [1] ++ 0.5 * abs r
        
        v :: Stream Word8
        v = externArrayW8 "varray" (abs idx) 9 Nothing + externArrayW8 "varray" idx 9 Nothing + externArrayW8 "varray" idx' 9 Nothing
          where idx :: Stream Word8
                idx  =  [1] ++ (1 + idx)
                idx' :: Stream Word8
                idx' =  [2] ++ (2 + idx)
        
        
scheme = proof $ do
  check "stayIdle"
  check "pv"