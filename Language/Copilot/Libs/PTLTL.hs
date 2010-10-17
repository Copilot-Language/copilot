-- | Provides past-time linear-temporal logic (ptLTL operators).  Currently
-- won't make unique tmp stream names if you use the same operator twice in one
-- stream definition.

module Language.Copilot.Libs.PTLTL(previous, alwaysBeen, eventuallyPrev, since) where

import Prelude ()
import qualified Prelude as P

import Language.Copilot.Core
import Language.Copilot.Language

-- | Did @s@ hold in the previous period?
previous :: Var -> Spec Bool -> Streams
previous v s =  do
     v .= [False] ++ s  
 
-- | Has @s@ always held?
alwaysBeen :: Var -> Spec Bool -> Streams
alwaysBeen v s = do
     tmp .= [True] ++ (var tmp) && s       
     v .= s && varB tmp
  where tmp = "alwaysBeenTmp_" P.++ v
    
-- | Did @s@ hold at some time in the past (including the current state)?
eventuallyPrev :: Var -> Spec Bool -> Streams
eventuallyPrev v s = do
     tmp .= [False] ++ (var tmp) || s
     v .= s || varB tmp
  where tmp = "eventuallyPrevTmp_" P.++ v

-- | Once @s2@ holds, in the following state (period), does @s1@ continuously hold?
since ::  Var -> (Spec Bool, Spec Bool) -> Streams
since v (s1, s2) = do
     tmp .= [False] ++ (s2 || (s1 &&  varB v)) 
     v .= varB tmp && s1
  where tmp = "sinceTmp_" P.++ v

