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

import Data.Word (Word8)
import Language.Atom

data Queue a = Queue
  { queueRingBuffer :: A a
  , queuePointer    :: V Word8
  , size            :: Word8
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
    p <== mux (value p + 1 >=. fromIntegral sz) 0 (value p + 1)

lookahead :: Expr a => Int -> Queue a -> E a
lookahead i
  Queue
    { queueRingBuffer = buf
    , queuePointer    = p
    , size            = sz
    } =
  let
    k = (value p + fromIntegral i + 1) `mod_` fromIntegral sz
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
