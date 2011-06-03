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

data Queue α = Queue
  { queueRingBuffer :: A α
  , queuePointer    :: V Word8
  , size            :: Word8
  }

dropFirstElemAndSnoc :: Assign α => E α -> Queue α -> Atom ()
dropFirstElemAndSnoc x
  Queue
    { queueRingBuffer = buf
    , queuePointer    = p
    , size            = sz
    } =
  do
    (buf ! value p) <== x
    p <== mux (value p + 1 >=. fromIntegral sz) 0 (value p + 1)

lookahead :: Expr α => Int -> Queue α -> E α
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

queue :: Expr α => String -> [α] -> Atom (Queue α)
queue name xs =
  do
    buf <- array ("queue_buffer_" ++ name) xs
    p   <- var   ("queue_pointer_" ++ name) 0
    return
      Queue
        { queueRingBuffer = buf
        , queuePointer    = p
        , size            = fromIntegral (length xs) }
