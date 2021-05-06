--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

-- | Boolean operators applied point-wise to streams.
module Copilot.Language.Operators.Boolean
  ( (&&)
  , (||)
  , not
  , true
  , false
  , xor
  , (==>)
  ) where

import qualified Copilot.Core as Core
import Copilot.Language.Prelude
import Copilot.Language.Operators.Constant (constant)
import Copilot.Language.Stream
import qualified Prelude as P

--------------------------------------------------------------------------------

-- | A stream that contains the constant value 'True'.
true :: Stream Bool
true = constant True

-- | A stream that contains the constant value 'False'.
false :: Stream Bool
false = constant False

infixr 4 &&

-- | Apply the and ('&&') operator to two boolean streams, point-wise.
(&&) :: Stream Bool -> Stream Bool -> Stream Bool
(Const False) && _ = false
_ && (Const False) = false
(Const True) && y  = y
x && (Const True)  = x
x && y             = Op2 Core.And x y

infixr 4 ||

-- | Apply the or ('||') operator to two boolean streams, point-wise.
(||) :: Stream Bool -> Stream Bool -> Stream Bool
(Const True) || _  = true
_ || (Const True)  = true
(Const False) || y = y
x || (Const False) = x
x || y             = Op2 Core.Or x y

-- | Negate all the values in a boolean stream.
not :: Stream Bool -> Stream Bool
not (Const c) = (Const $ P.not c)
not x         = Op1 Core.Not x

-- | Apply the exclusive-or ('xor') operator to two boolean streams,
-- point-wise.
xor :: Stream Bool -> Stream Bool -> Stream Bool
xor x y = ( not x && y ) || ( x && not y )

-- | Apply the implication ('==>') operator to two boolean streams, point-wise.
(==>) :: Stream Bool -> Stream Bool -> Stream Bool
x ==> y = not x || y

--------------------------------------------------------------------------------
