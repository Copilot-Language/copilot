module Copilot.Compile.C99.Util where

import Control.Monad.State

import Copilot.Core  (Id)
import qualified Language.C99.Simple.AST as C


type FunEnv = ([C.Decln], [C.Ident])

-- | `tell` equivalent for `State`.
statetell :: Monoid m => m -> State m ()
statetell m = modify ((flip mappend) m)

-- | Generate fresh variable name based on a given one.
fresh :: String -> [String] -> String
fresh name used = head $ dropWhile (flip elem used) (name:freshnames) where
  freshnames = (name ++).show <$> [0..]

-- | Collect all the names from a list of C99 declarations.
names :: [C.Decln] -> [String]
names ds = map match ds where
  match (C.VarDecln _ _ name _) = name

-- | Turn a stream id into a suitable C variable name.
streamname :: Id -> String
streamname sid = "s" ++ show sid

-- | Turn a stream id into the global varname for indices.
indexname :: Id -> String
indexname sid = streamname sid ++ "_idx"

-- | Add a postfix for copies of external variables the name.
excpyname :: String -> String
excpyname name = name ++ "_cpy"

-- | Turn stream id into name of its generator function.
generatorname :: Id -> String
generatorname sid = streamname sid ++ "_gen"

-- | Turn a the name of a trigger into a guard generator.
guardname :: String -> String
guardname name = name ++ "_guard"

-- | Turn a trigger name into a an trigger argument name.
argname :: String -> Int -> String
argname name n = name ++ "_arg" ++ show n

-- | Enumerate all argument names based on trigger name.
argnames :: String -> [String]
argnames base = [aname | n <- [0..], let aname = argname base n]

funcall :: C.Ident -> [C.Expr] -> C.Expr
funcall name args = C.Funcall (C.Ident name) args
