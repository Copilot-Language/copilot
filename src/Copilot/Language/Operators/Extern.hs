--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Extern
  ( FunArg
  , extern
  , externFun
  , externArray
  , funArg
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream

--------------------------------------------------------------------------------

extern :: Typed a => String -> Stream a
extern = Extern

externFun :: Typed a => String -> [FunArg] -> Stream a
externFun = undefined

externArray :: (Typed a, Typed b, Integral a) => String -> Stream a -> Stream b
externArray = undefined

funArg :: Typed a => Stream a -> FunArg
funArg = FunArg

--------------------------------------------------------------------------------
