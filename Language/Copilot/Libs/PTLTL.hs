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

import Prelude (String, ($), error)
import qualified Prelude as P

import Language.Copilot.Core
import Language.Copilot.Language

tmpName :: Spec Bool -> String -> Spec Bool
tmpName v name = 
  case v of
    Var v' -> varB (v' P.++ "_" P.++ name)
    _     -> error "Copilot error in tmpName in PTLTL.hs."

ptltl :: Spec Bool -> (Spec Bool -> Streams) -> Streams
ptltl v f = f v

-- | Did @s@ hold in the previous period?
previous :: Spec Bool -> Spec Bool -> Streams
previous s v = v .= [False] ++ s  

-- | Has @s@ always held (up to and including the current state)?
alwaysBeen :: Spec Bool -> Spec Bool -> Streams
alwaysBeen s v = do
     tmp .= [True] ++ v
     v   .= tmp && s'
     s'  .= s
  where 
    tmp = tmpName v "ab_tmp"
    s'  = tmpName v "ab"
    
-- | Did @s@ hold at some time in the past (including the current state)?
eventuallyPrev :: Spec Bool -> Spec Bool -> Streams
eventuallyPrev s v = do
     tmp .= [False] ++ tmp || s'
     v   .= s' || tmp
     s'  .= s
  where 
    tmp = tmpName v "ep_tmp"
    s'  = tmpName v "ep"

-- | Once @s2@ holds, in the following state (period), does @s1@ continuously hold? 
since ::  Spec Bool -> Spec Bool -> Spec Bool -> Streams
since s1 s2 v = do
    tmp `ptltl` (eventuallyPrev $ [False] ++ s2) -- has s2 been true at some point?
    v   `ptltl` (alwaysBeen $ tmp ==> s1')
    s1' .= s1
    s2' .= s2
  where 
    tmp  = tmpName v "since_tmp"
    s1'  = tmpName v "since1"
    s2'  = tmpName v "since2"
