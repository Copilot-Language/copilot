-- | Auxiliary helper functions to generate C99 code.
module Copilot.Compile.C99.Util where

import Control.Monad.State

import Copilot.Core  (Id)
import qualified Language.C99.Simple.AST as C

-- | Auxiliary type used to collect all the declarations of all the variables
-- used in a function to be generated, since variable declarations are always
-- listed first at the top of the function body.
type FunEnv = [C.Decln]

-- | `tell` equivalent for `State`.
stateTell :: Monoid m => m -> State m ()
stateTell m = modify ((flip mappend) m)

-- | Generate fresh variable name based on a given one.
fresh :: String -> [String] -> String
fresh name used = head $ dropWhile (flip elem used) (name:freshNames)
  where
    freshNames = (name ++).show <$> [0..]

-- | Collect all the names from a list of C99 declarations.
names :: [C.Decln] -> [String]
names ds = map match ds
  where
    match (C.VarDecln _ _ name _) = name

-- | Turn a stream id into a suitable C variable name.
streamName :: Id -> String
streamName sId = "s" ++ show sId

-- | Turn a stream id into the global varname for indices.
indexName :: Id -> String
indexName sId = streamName sId ++ "_idx"

-- | Turn a stream id into the name of its accessor function
streamAccessorName :: Id -> String
streamAccessorName sId = streamName sId ++ "_get"

-- | Add a postfix for copies of external variables the name.
exCpyName :: String -> String
exCpyName name = name ++ "_cpy"

-- | Turn stream id into name of its generator function.
generatorName :: Id -> String
generatorName sId = streamName sId ++ "_gen"

-- | Turn stream id into name of its output argument array.
generatorOutputArgName :: Id -> String
generatorOutputArgName sId = streamName sId ++ "_output"

-- | Turn the name of a trigger into a guard generator.
guardName :: String -> String
guardName name = name ++ "_guard"

-- | Turn a trigger name into a an trigger argument name.
argName :: String -> Int -> String
argName name n = name ++ "_arg" ++ show n

-- | Turn a handler function name into a name for a temporary variable for a
-- handler argument.
argTempName :: String -> Int -> String
argTempName name n = name ++ "_arg_temp" ++ show n

-- | Enumerate all argument names based on trigger name.
argNames :: String -> [String]
argNames base = [aName | n <- [0..], let aName = argName base n]

-- | Enumerate all temporary variable names based on handler function name.
argTempNames :: String -> [String]
argTempNames base = map (argTempName base) [0..]

-- | Define a C expression that calls a function with arguments.
funCall :: C.Ident -> [C.Expr] -> C.Expr
funCall name args = C.Funcall (C.Ident name) args
