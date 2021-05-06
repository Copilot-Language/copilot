--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Copilot specification sanity check.

module Copilot.Language.Analyze
  ( AnalyzeException (..)
  , analyze
  ) where

import Copilot.Core (DropIdx)
import qualified Copilot.Core as C
import Copilot.Language.Stream (Stream (..), Arg (..))
import Copilot.Language.Spec
import Copilot.Language.Error (badUsage)

import Data.List (groupBy)
import Data.IORef
import Data.Typeable
import System.Mem.StableName.Dynamic
import System.Mem.StableName.Map (Map(..))
import qualified System.Mem.StableName.Map as M
import Control.Monad (when, foldM_, foldM)
import Control.Exception (Exception, throw)

--------------------------------------------------------------------------------

-- | Exceptions or kinds of errors in Copilot specifications that the analysis
-- implemented is able to detect.
data AnalyzeException
  = DropAppliedToNonAppend
  | DropIndexOverflow
  | ReferentialCycle
  | DropMaxViolation
  | NestedArray
  | TooMuchRecursion
  | InvalidField
  | DifferentTypes String
  | Redeclared String
  | BadNumberOfArgs String
  | BadFunctionArgType String
  deriving Typeable

-- | Show instance that prints a detailed message for each kind of exception.
instance Show AnalyzeException where
  show DropAppliedToNonAppend = badUsage $  "Drop applied to non-append operation!"
  show DropIndexOverflow      = badUsage $  "Drop index overflow!"
  show ReferentialCycle       = badUsage $  "Referential cycle!"
  show DropMaxViolation       = badUsage $  "Maximum drop violation (" ++
                                  show (maxBound :: DropIdx) ++ ")!"
  show NestedArray            = badUsage $
    "An external function cannot take another external function or external array as an argument.  Try defining a stream, and using the stream values in the other definition."
  show TooMuchRecursion       = badUsage $
    "You have exceeded the limit of " ++ show maxRecursion ++ " recursive calls in a stream definition.  Likely, you have accidently defined a circular stream, such as 'x = x'.  Another possibility is you have defined a a polymorphic function with type constraints that references other streams.  For example,\n\n  nats :: (Typed a, Num a) => Stream a\n  nats = [0] ++ nats + 1\n\nis not allowed.  Make the definition monomorphic, or add a level of indirection, like \n\n  nats :: (Typed a, Num a) => Stream a\n  nats = n\n    where n = [0] ++ n + 1\n\nFinally, you may have intended to generate a very large expression.  You can try shrinking the expression by using local variables.  It all else fails, you can increase the maximum size of ecursive calls by modifying 'maxRecursion' in copilot-language."
  show InvalidField           = badUsage $
    "A struct can only take external variables, arrays, or other structs as fields."
  show (DifferentTypes name) = badUsage $
    "The external symbol \'" ++ name ++ "\' has been declared to have two different types!"
  show (Redeclared name) = badUsage $
    "The external symbol \'" ++ name ++ "\' has been redeclared to be a different symbol (e.g., a variable and an array, or a variable and a funciton symbol, etc.)."
  show (BadNumberOfArgs name) = badUsage $
    "The function symbol \'" ++ name ++ "\' has been redeclared to have different number of arguments."
  show (BadFunctionArgType name) = badUsage $
    "The function symbol \'" ++ name ++ "\' has been redeclared to an argument with different types."

-- | 'Exception' instance so we can throw and catch 'AnalyzeExcetion's.
instance Exception AnalyzeException

-- | Max level of recursion supported. Any level above this constant
-- will result in a 'TooMuchRecursion' exception.
maxRecursion :: Int
maxRecursion = 100000

--------------------------------------------------------------------------------

-- | An environment that contains the nodes visited.
type Env = Map ()

--------------------------------------------------------------------------------

-- | Analyze a Copilot specification and report any errors detected.
--
-- This function can fail with one of the exceptions in 'AnalyzeException'.
analyze :: Spec' a -> IO ()
analyze spec = do
  refStreams <- newIORef M.empty
  mapM_ (analyzeTrigger  refStreams) (triggers  $ runSpec spec)
  mapM_ (analyzeObserver refStreams) (observers $ runSpec spec)
  mapM_ (analyzeProperty refStreams) (properties $ runSpec spec)
  mapM_ (analyzeProperty refStreams) (map fst $ theorems $ runSpec spec)
  specExts refStreams spec >>= analyzeExts

--------------------------------------------------------------------------------

-- | Analyze a Copilot trigger and report any errors detected.
--
-- This function can fail with one of the exceptions in 'AnalyzeException'.
analyzeTrigger :: IORef Env -> Trigger -> IO ()
analyzeTrigger refStreams (Trigger _ e0 args) =
  analyzeExpr refStreams e0 >> mapM_ analyzeTriggerArg args

  where
  analyzeTriggerArg :: Arg -> IO ()
  analyzeTriggerArg (Arg e) = analyzeExpr refStreams e

--------------------------------------------------------------------------------

-- | Analyze a Copilot observer and report any errors detected.
--
-- This function can fail with one of the exceptions in 'AnalyzeException'.
analyzeObserver :: IORef Env -> Observer -> IO ()
analyzeObserver refStreams (Observer _ e) = analyzeExpr refStreams e

--------------------------------------------------------------------------------

-- | Analyze a Copilot property and report any errors detected.
--
-- This function can fail with one of the exceptions in 'AnalyzeException'.
analyzeProperty :: IORef Env -> Property -> IO ()
analyzeProperty refStreams (Property _ e) = analyzeExpr refStreams e

--------------------------------------------------------------------------------

data SeenExtern = NoExtern
                | SeenFun
                | SeenArr
                | SeenStruct

--------------------------------------------------------------------------------

-- | Analyze a Copilot stream and report any errors detected.
--
-- This function can fail with one of the exceptions in 'AnalyzeException'.
analyzeExpr :: IORef Env -> Stream a -> IO ()
analyzeExpr refStreams s = do
  b <- mapCheck refStreams
  when b (throw TooMuchRecursion)
  go NoExtern M.empty s

  where
  go :: SeenExtern -> Env -> Stream b -> IO ()
  go seenExt nodes e0 = do
    dstn <- makeDynStableName e0
    assertNotVisited e0 dstn nodes
    let nodes' = M.insert dstn () nodes
    case e0 of
      Append _ _ e        -> analyzeAppend refStreams dstn e () analyzeExpr
      Const _             -> return ()
      Drop k e1           -> analyzeDrop (fromIntegral k) e1
      Extern _ _          -> return ()
      Local e f           -> go seenExt nodes' e >>
                             go seenExt nodes' (f (Var "dummy"))
      Var _               -> return ()
      Op1 _ e             -> go seenExt nodes' e
      Op2 _ e1 e2         -> go seenExt nodes' e1 >>
                             go seenExt nodes' e2
      Op3 _ e1 e2 e3      -> go seenExt nodes' e1 >>
                             go seenExt nodes' e2 >>
                             go seenExt nodes' e3
      Label _ e           -> go seenExt nodes' e

--------------------------------------------------------------------------------

-- | Detect whether the given stream name has already been visited.
--
-- This function throws a 'ReferentialCycle' exception if the second argument
-- represents a name that has already been visited.
assertNotVisited :: Stream a -> DynStableName -> Env -> IO ()
assertNotVisited (Append _ _ _) _    _     = return ()
assertNotVisited _              dstn nodes =
  case M.lookup dstn nodes of
    Just () -> throw ReferentialCycle
    Nothing -> return ()

--------------------------------------------------------------------------------

-- | Check that the level of recursion is not above the max recursion allowed.
mapCheck :: IORef Env -> IO Bool
mapCheck refStreams = do
  ref <- readIORef refStreams
  return $ getSize ref > maxRecursion

--------------------------------------------------------------------------------

-- | Analyze a Copilot stream append and report any errors detected.
analyzeAppend ::
     IORef Env -> DynStableName -> Stream a -> b
  -> (IORef Env -> Stream a -> IO b) -> IO b
analyzeAppend refStreams dstn e b f = do
  streams <- readIORef refStreams
  case M.lookup dstn streams of
    Just () -> return b
    Nothing -> do
      modifyIORef refStreams $ M.insert dstn ()
      f refStreams e

--------------------------------------------------------------------------------

-- | Analyze a Copilot stream drop and report any errors detected.
--
-- This function can fail if the drop is exercised over a stream that is not an
-- append, or if the length of the drop is larger than that of the subsequent
-- append.
analyzeDrop :: Int -> Stream a -> IO ()
analyzeDrop k (Append xs _ _)
  | k >= length xs                         = throw DropIndexOverflow
  | k > fromIntegral (maxBound :: DropIdx) = throw DropMaxViolation
  | otherwise                              = return ()
analyzeDrop _ _                            = throw DropAppliedToNonAppend


--------------------------------------------------------------------------------
-- Analyzing external variables.  We check that every reference to an external
-- variable has the same type, and for external functions, they have the same
-- typed arguments.
--------------------------------------------------------------------------------

-- | An environment to store external variables, arrays, functions and structs,
-- so that we can check types in the expression---e.g., if we declare the same
-- external to have two different types.
data ExternEnv = ExternEnv
  { externVarEnv  :: [(String, C.SimpleType)]
  , externArrEnv  :: [(String, C.SimpleType)]
  , externStructEnv  :: [(String, C.SimpleType)]
  , externStructArgs :: [(String, [C.SimpleType])]
  }

--------------------------------------------------------------------------------

-- | Make sure external variables, functions, arrays, and structs are correctly
-- typed.
analyzeExts :: ExternEnv -> IO ()
analyzeExts ExternEnv { externVarEnv  = vars
                      , externArrEnv  = arrs
                      , externStructEnv  = datastructs
                      , externStructArgs = struct_args }
    = do
    -- symbol names redeclared?
    findDups vars arrs
    --findDups vars struct_args
    findDups vars datastructs
    --findDups arrs struct_args
    findDups arrs datastructs
    -- conflicting types?
    conflictingTypes vars
    conflictingTypes arrs
    -- symbol names given different number of args and right types?
    --funcArgCheck struct_args
    funcArgCheck struct_args

  where
  findDups :: [(String, a)] -> [(String, b)] -> IO ()
  findDups ls0 ls1 = mapM_ (\(name,_) -> dup name) ls0
    where
    dup nm = mapM_ ( \(name',_) -> if name' == nm
                                     then throw (Redeclared nm)
                                     else return ()
                   ) ls1

  conflictingTypes :: [(String, C.SimpleType)] -> IO ()
  conflictingTypes ls =
    let grps = groupByPred ls in
    mapM_ sameType grps
    where
    sameType :: [(String, C.SimpleType)] -> IO ()
    sameType grp = foldCheck check grp
    check name c0 c1 = if c0 == c1 then return (name,c0) -- a dummy---we
                                                         -- discard the result
                         else throw (DifferentTypes name)

  funcArgCheck :: [(String, [C.SimpleType])] -> IO ()
  funcArgCheck ls =
    let grps = groupByPred ls in
    mapM_ argCheck grps
    where
    argCheck :: [(String, [C.SimpleType])] -> IO ()
    argCheck grp = foldCheck check grp
    check name args0 args1 =
      if length args0 == length args1
        then if args0 == args1
               then return (name,args0) -- a dummy---we discard the
                                        -- result
               else throw (BadFunctionArgType name)
        else throw (BadNumberOfArgs name)

  {-structArgCheck :: [(String, [C.SimpleType])] -> IO ()
  structArgCheck ls = foldr (\sarg' _ -> findDups (getArgName sarg', sarg') (getArgName sarg', sarg'))
                        (return ()) $ map snd ls-}

  groupByPred :: [(String, a)] -> [[(String, a)]]
  groupByPred = groupBy (\(n0,_) (n1,_) -> n0 == n1)

  foldCheck :: (String -> a -> a -> IO (String, a)) -> [(String, a)] -> IO ()
  foldCheck check grp =
    foldM_ ( \(name, c0) (_, c1) -> check name c0 c1)
           (head grp) -- should be typesafe, since this is from groupBy
           grp

--------------------------------------------------------------------------------

-- | Obtain all the externs in a specification.
specExts :: IORef Env -> Spec' a -> IO ExternEnv
specExts refStreams spec = do
  env <- foldM triggerExts
           (ExternEnv [] [] [] [])
           (triggers $ runSpec spec)
  foldM observerExts env (observers $ runSpec spec)

  where
  observerExts :: ExternEnv -> Observer -> IO ExternEnv
  observerExts env (Observer _ stream) = collectExts refStreams stream env

  triggerExts :: ExternEnv -> Trigger -> IO ExternEnv
  triggerExts env (Trigger _ guard args) = do
    env' <- collectExts refStreams guard env
    foldM (\env'' (Arg arg_) -> collectExts refStreams arg_ env'')
          env' args

-- | Obtain all the externs in a stream.
collectExts :: C.Typed a => IORef Env -> Stream a -> ExternEnv -> IO ExternEnv
collectExts refStreams stream_ env_ = do
  b <- mapCheck refStreams
  when b (throw TooMuchRecursion)
  go M.empty env_ stream_

  where
  go :: Env -> ExternEnv -> Stream b -> IO ExternEnv
  go nodes env stream = do
    dstn <- makeDynStableName stream
    assertNotVisited stream dstn nodes

    case stream of
      Append _ _ e           -> analyzeAppend refStreams dstn e env
                                  (\refs str -> collectExts refs str env)
      Const _                -> return env
      Drop _ e1              -> go nodes env e1
      Extern name _          ->
        let ext = ( name, getSimpleType stream ) in
        return env { externVarEnv = ext : externVarEnv env }

      Local e _              -> go nodes env e
      Var _                  -> return env
      Op1 _ e                -> go nodes env e
      Op2 _ e1 e2            -> do env' <- go nodes env e1
                                   go nodes env' e2
      Op3 _ e1 e2 e3         -> do env' <- go nodes env e1
                                   env'' <- go nodes env' e2
                                   go nodes env'' e3
      Label _ e              -> go nodes env e

--------------------------------------------------------------------------------

-- | Return the simple C type representation of the type of the values carried
-- by a stream.
getSimpleType :: forall a. C.Typed a => Stream a -> C.SimpleType
getSimpleType _ = C.simpleType (C.typeOf :: C.Type a)

--------------------------------------------------------------------------------
