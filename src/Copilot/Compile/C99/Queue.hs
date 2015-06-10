--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.Queue
  ( Queue
  , dropFirstElemAndSnoc
  , lookahead
  , size
  , queue
  ) where

import Data.Word (Word16)
import Language.Atom

type QueueIndexType = Word16

data Queue a = Queue
  { queueRingBuffer :: A a
  , queuePointer    :: V QueueIndexType
  , size            :: QueueIndexType
  }

dropFirstElemAndSnoc :: Assign a => E a -> Queue a -> Atom ()
dropFirstElemAndSnoc x
  Queue
    { queueRingBuffer = buf
    , queuePointer    = p
    , size            = sz
    } =
  do
    (buf ! value p) <== x
    p <== (value p + 1) `mod_` fromIntegral sz

lookahead :: Expr a => Int -> Queue a -> E a
lookahead i
  Queue
    { queueRingBuffer = buf
    , queuePointer    = p
    , size            = sz
    } =
  let
    k = (value p + fromIntegral i) `mod_` fromIntegral sz
  in
    buf !. k

queue :: Expr a => String -> [a] -> Atom (Queue a)
queue name xs =
  do
    buf <- array ("queue_buffer_" ++ name) xs
    p   <- var   ("queue_pointer_" ++ name) 0
    return
      Queue
        { queueRingBuffer = buf
        , queuePointer    = p
        , size            = fromIntegral (length xs) }
