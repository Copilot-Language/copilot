--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Stream construction.

{-# LANGUAGE Trustworthy #-}

module Copilot.Language.Operators.Temporal
  ( (++)
  , drop
  , (#)
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Prelude
import Copilot.Language.Stream
import Prelude ()

--------------------------------------------------------------------------------

infixr 1 ++

(++) :: Typed a => [a] -> Stream a -> Stream a
(++) = (`Append` Nothing)

drop :: Typed a => Int -> Stream a -> Stream a
drop 0 s             = s
drop _ ( Const j )   = Const j
drop i ( Drop  j s ) = Drop (fromIntegral i + j) s
drop i s             = Drop (fromIntegral i)     s

(#) :: (Typed a, Typed b) => Stream a -> String -> Stream b
(#) = GetField
{-(ExternStruct cs sargs) # (Extern nm i) 		 				= Extern nm i
(ExternStruct cs sargs) # (ExternFun nm args i) 		= ExternFun nm args i
(ExternStruct cs sargs) # (ExternArray nm strm j i)	= ExternArray nm strm j i
(ExternStruct cs sargs) # (ExternStruct nm args)	  = ExternStruct nm args-}
--(ExternStruct cs sargs) # name = GetField (ExternStruct cs sargs) name
