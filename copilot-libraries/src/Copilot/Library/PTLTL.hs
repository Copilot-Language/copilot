-- |
-- Module: PTLTL
-- Description: Past-Time Linear-Temporal Logic
-- Copyright: (c) 2011 National Institute of Aerospace / Galois, Inc.
--
-- Provides past-time linear-temporal logic (ptLTL operators).
--
-- /Interface:/ See @Examples/PTLTLExamples.hs@ in the
-- <https://github.com/Copilot-Language/copilot/blob/v2.2.1/Examples Copilot repository>.
--
-- You can embed a ptLTL specification within a Copilot specification using
-- the form:
--
-- @
--   operator stream
-- @

{-# LANGUAGE NoImplicitPrelude #-}

module Copilot.Library.PTLTL
    ( since, alwaysBeen, eventuallyPrev, previous ) where

import Prelude ()
import Copilot.Language

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
  where tmp = [ False ] ++ s || tmp

-- | Once @s2@ holds, in the following state (period), does @s1@ continuously hold?
since ::  Stream Bool -> Stream Bool -> Stream Bool
since s1 s2 = alwaysBeen ( tmp ==> s1 )
    where tmp = eventuallyPrev $ [ False ] ++ s2
