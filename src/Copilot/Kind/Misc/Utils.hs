--------------------------------------------------------------------------------

module Copilot.Kind.Misc.Utils 
 ( module Data.Maybe
 , module Control.Monad
 , (<>)
 , (<$>)
 , nub, partition, groupBy, sortBy, intercalate, find, (\\)
 , fst3, snd3, thrd3
 , on
 , (!)
 , Map 
 ) where


import Data.Maybe
import Data.Monoid ((<>))

import Data.Function (on)
import Data.List (nub, partition, groupBy, sortBy, intercalate, find, (\\))

import Control.Applicative ((<$>))
import Control.Monad

import Data.Map ((!), Map)

fst3  (a, _, _) = a
snd3  (_, b, _) = b
thrd3 (_, _, c) = c

--------------------------------------------------------------------------------
