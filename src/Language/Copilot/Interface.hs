-- |

module Language.Copilot.Interface
  ( module Data.Int
  , module Data.Word
  , Stream
  , Streamable
  , interpret
  , (++)
  , (&&), (||)
  , (==), (/=)
  , (<=), (>=), (<), (>)
  , const
  , drop
  , extern
  , not
  , mod
  , mux
  , true
  , false
  ) where

import Data.Int
import Data.Word
import Language.Copilot.Core (Streamable, Fun1 (..), Fun2 (..), Fun3 (..))
import Language.Copilot.Interface.Prelude
import Language.Copilot.Interface.Reify (reify)
import Language.Copilot.Interface.Stream (Stream (..))
import qualified Language.Copilot.Interpret as I
import qualified Prelude as P

infixr 3 ++

(++) :: Streamable a => [a] -> Stream a -> Stream a
(++) = Append

const :: Streamable a => a -> Stream a
const = Const

drop :: Streamable a => Int -> Stream a -> Stream a
drop = Drop

extern :: Streamable a => String -> Stream a
extern = Extern

not :: Stream Bool -> Stream Bool
not = Fun1 Not

(&&) :: Stream Bool -> Stream Bool -> Stream Bool
(&&) = Fun2 And

(||) :: Stream Bool -> Stream Bool -> Stream Bool
(||) = Fun2 Or

(==) :: (Streamable a, P.Eq a) => Stream a -> Stream a -> Stream Bool
(==) = Fun2 Eq

(/=) :: (Streamable a, P.Eq a) => Stream a -> Stream a -> Stream Bool
(/=) = Fun2 Ne

(<=) :: (Streamable a, P.Ord a) => Stream a -> Stream a -> Stream Bool
(<=) = Fun2 Le

(>=) :: (Streamable a, P.Ord a) => Stream a -> Stream a -> Stream Bool
(>=) = Fun2 Ge

(<) :: (Streamable a, P.Ord a) => Stream a -> Stream a -> Stream Bool
(<) = Fun2 Lt

(>) :: (Streamable a, P.Ord a) => Stream a -> Stream a -> Stream Bool
(>) = Fun2 Gt

mod :: (Streamable a, Integral a) => Stream a -> Stream a -> Stream a
mod = Fun2 Mod

mux :: Streamable a => Stream Bool -> Stream a -> Stream a -> Stream a
mux = Fun3 Mux

true :: Stream Bool
true = Const True

false :: Stream Bool
false = Const False

interpret
  :: Streamable a
  => Integer
  -> Stream a
  -> IO ()
interpret i e =
  do
    sp <- reify e
    let xs = take (fromInteger i) $ I.interpret sp
    print xs
