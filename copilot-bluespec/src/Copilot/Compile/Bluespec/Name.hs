-- | Naming of variables and functions in Bluespec.
module Copilot.Compile.Bluespec.Name
  ( argNames
  , generatorName
  , guardName
  , ifcArgName
  , indexName
  , lowercaseName
  , specIfcName
  , specIfcPkgName
  , specTypesPkgName
  , streamAccessorName
  , streamElemName
  , streamName
  , uppercaseName
  ) where

-- External imports
import Data.Char (isLower, isUpper)

-- External imports: Copilot
import Copilot.Core (Id)

-- | Turn a specification name into the name of its module interface.
specIfcName :: String -> String
specIfcName prefix = uppercaseName (specIfcPkgName prefix)

-- | Turn a specification name into the name of the package that declares its
-- module interface. Note that 'specIfcPkgName' is not necessarily the same name
-- as 'specIfcName', as the former does not need to begin with an uppercase
-- letter, but the latter does.
specIfcPkgName :: String -> String
specIfcPkgName prefix = prefix ++ "Ifc"

-- | Turn a specification name into the name of the package that declares its
-- struct types.
specTypesPkgName :: String -> String
specTypesPkgName prefix = prefix ++ "Types"

-- | Turn a stream id into a stream element name.
streamElemName :: Id -> Int -> String
streamElemName sId n = streamName sId ++ "_" ++ show n

-- | The name of the variable of type @<prefix>Ifc@. This is used to select
-- trigger functions and external variables.
ifcArgName :: String
ifcArgName = "ifc"

-- | Create a Bluespec name that must start with an uppercase letter (e.g., a
-- struct or interface name). If the supplied name already begins with an
-- uppercase letter, this function returns the name unchanged. Otherwise, this
-- function prepends a @BS_@ prefix (short for \"Bluespec\") at the front.
uppercaseName :: String -> String
uppercaseName [] = []
uppercaseName n@(c:_)
  | isUpper c = n
  | otherwise = "BS_" ++ n

-- | Create a Bluespec name that must start with a lowercase letter (e.g., a
-- function or method name). If the supplied name already begins with a
-- lowercase letter, this function returns the name unchanged. Otherwise, this
-- function prepends a @bs_@ prefix (short for \"Bluespec\") at the front.
lowercaseName :: String -> String
lowercaseName [] = []
lowercaseName n@(c:_)
  | isLower c = n
  | otherwise = "bs_" ++ n

-- | Turn a stream id into a suitable Bluespec variable name.
streamName :: Id -> String
streamName sId = "s" ++ show sId

-- | Turn a stream id into the global varname for indices.
indexName :: Id -> String
indexName sId = streamName sId ++ "_idx"

-- | Turn a stream id into the name of its accessor function
streamAccessorName :: Id -> String
streamAccessorName sId = streamName sId ++ "_get"

-- | Turn stream id into name of its generator function.
generatorName :: Id -> String
generatorName sId = streamName sId ++ "_gen"

-- | Turn the name of a trigger into a guard generator.
guardName :: String -> String
guardName name = lowercaseName name ++ "_guard"

-- | Turn a trigger name into a an trigger argument name.
argName :: String -> Int -> String
argName name n = lowercaseName name ++ "_arg" ++ show n

-- | Enumerate all argument names based on trigger name.
argNames :: String -> [String]
argNames base = map (argName base) [0..]
