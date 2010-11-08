-- | Provides past-time linear-temporal logic (ptLTL operators).
-- 
-- Interface: see Examples/PTLTLExamples.hs, particularly function tSinExt2 in
-- that file.  You can embed a ptLTL specification within a Copilot
-- specification using the form:
-- @
--   var `ptltl` (operator spec)
-- @
-- where 'var' is the variable you want to assign to the ptLTL specification
-- you're writing.

module Language.Copilot.Libs.PTLTL 
  ( ptltl, since, alwaysBeen, eventuallyPrev, previous ) 
  where

import Prelude (String, ($))
import qualified Prelude as P

import Language.Copilot.Core
import Language.Copilot.Language

tmpName :: Var -> String -> Var
tmpName v name = v P.++ "_" P.++ name

ptltl :: Var -> (Var -> Streams) -> Streams
ptltl v f = f v

-- | Did @s@ hold in the previous period?
previous :: Spec Bool -> Var -> Streams
previous s v = v .= [False] ++ s  

-- | Has @s@ always held (up to and including the current state)?
alwaysBeen :: Spec Bool -> Var -> Streams
alwaysBeen s v = do
     tmp .= [True] ++ varB v
     v   .= varB tmp && varB s'
     s'  .= s
  where 
    tmp = tmpName v "ab_tmp"
    s'  = tmpName v "ab"
    
-- | Did @s@ hold at some time in the past (including the current state)?
eventuallyPrev :: Spec Bool -> Var -> Streams
eventuallyPrev s v = do
     tmp .= [False] ++ (var tmp) || varB s'
     v   .= varB s' || varB tmp
     s'  .= s
  where 
    tmp = tmpName v "ep_tmp"
    s'  = tmpName v "ep"

-- | Once @s2@ holds, in the following state (period), does @s1@ continuously hold? 
since ::  Spec Bool -> Spec Bool -> Var -> Streams
since s1 s2 v = do
    tmp `ptltl` (eventuallyPrev $ [False] ++ s2) -- has s2 been true at some point?
    v   `ptltl` (alwaysBeen $ varB tmp ==> varB s1')
    s1' .= s1
    s2' .= s2
  where 
    tmp  = tmpName v "since_tmp"
    s1'  = tmpName v "since1"
    s2'  = tmpName v "since2"
