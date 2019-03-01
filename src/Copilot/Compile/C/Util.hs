module Copilot.Compile.C.Util where

import Control.Monad.State

import Copilot.Core  (Id)
import qualified Language.C99.Simple.AST as C


type FunEnv = ([C.Decln], [C.Ident])

-- | `tell` equivalent for `State`.
statetell :: Monoid m => m -> State m ()
statetell m = modify (mappend m)

-- | Generate fresh variable name based on a given one.
fresh :: String -> [String] -> String
fresh name used = head $ dropWhile (flip elem used) (name:freshnames) where
  freshnames = (name ++).show <$> [0..]

-- | Collect all the names from a list of C99 declarations.
names :: [C.Decln] -> [String]
names ds = map match ds where
  match (C.Decln _ _ name _) = name

-- | Turn a stream id into a suitable C variable name.
streamname :: Id -> String
streamname sid = "s" ++ show sid

-- | Add a postifx for the buffer
buffername :: Id -> String
buffername sid = streamname sid ++ "_buff"

-- | Turn a stream id into the global varname for indices.
indexname :: Id -> String
indexname sid = streamname sid ++ "_idx"

-- | Add a postfix for copies of external variables the name.
excpyname :: String -> String
excpyname name = name ++ "_cpy"

funcall :: C.Ident -> [C.Expr] -> C.Expr
funcall name args = C.Funcall (C.Ident name) args
