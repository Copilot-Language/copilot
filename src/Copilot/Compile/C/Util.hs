module Copilot.Compile.C.Util where

import Control.Monad.State

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

-- | Add a postfix for copies of external variables the name.
excpyname :: String -> String
excpyname name = name ++ "_cpy"

-- | Add a postfix for the `drop` operation on a stream, including the number
-- of dropped items, to a variables the name.
dropname :: String -> Int -> String
dropname name n = name ++ "_drop" ++ show n

funcall :: C.Ident -> [C.Expr] -> C.Expr
funcall name args = C.Funcall (C.Ident name) args
