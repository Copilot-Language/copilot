-- | Provides past-time linear-temporal logic (ptLTL operators).
--
-- Interface: see Examples/PTLTLExamples.hs.
-- You can embed a ptLTL specification within a Copilot specification using
-- the form:
-- @
--   operator stream
-- @

module Copilot.Library.PTLTL
    ( since, alwaysBeen, eventuallyPrev, previous ) where


import Prelude ( ($) )
import Copilot.Language
import Data.Bool hiding ( (&&), (||) )


-- | Did @s@ hold in the previous period?
previous :: Stream Bool -> Stream Bool
previous s = [ False ] ++ s


-- | Has @s@ always held (up to and including the current state)?
alwaysBeen :: Stream Bool -> Stream Bool
alwaysBeen s = s && tmp
    where tmp = [ True ] ++ s && tmp


-- | Did @s@ hold at some time in the past (including the current state)?
eventuallyPrev :: Stream Bool -> Stream Bool
eventuallyPrev s = s || tmp
  where tmp = [ False ] ++ tmp || s


-- | Once @s2@ holds, in the following state (period), does @s1@ continuously hold?
since ::  Stream Bool -> Stream Bool -> Stream Bool
since s1 s2 = alwaysBeen ( tmp ==> s1 )
    where tmp = eventuallyPrev $ [ False ] ++ s2
