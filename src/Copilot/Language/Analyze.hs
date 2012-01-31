--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

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
import System.Mem.StableName.Dynamic.Map (Map(..))
import qualified System.Mem.StableName.Dynamic.Map as M
import Control.Monad (when, foldM_, foldM)
import Control.Exception (Exception, throw)

--------------------------------------------------------------------------------

data AnalyzeException
  = DropAppliedToNonAppend
  | DropIndexOverflow
  | ReferentialCycle
  | DropMaxViolation
  | NestedExternFun
  | NestedArray
  | TooMuchRecussion
  | DifferentTypes String
  | Redeclared String
  | BadNumberOfArgs String
  | BadFunctionArgType String
  deriving Typeable

instance Show AnalyzeException where
  show DropAppliedToNonAppend = badUsage $  "Drop applied to non-append operation!"
  show DropIndexOverflow      = badUsage $  "Drop index overflow!"
  show ReferentialCycle       = badUsage $  "Referential cycle!"
  show DropMaxViolation       = badUsage $  "Maximum drop violation (" ++ 
                                  show (maxBound :: DropIdx) ++ ")!"
  show NestedExternFun        = badUsage $  
    "An external function cannot take another external function or external array as an argument.  Try defining a stream, and using the stream values in the other definition."
  show NestedArray            = badUsage $  
    "An external function cannot take another external function or external array as an argument.  Try defining a stream, and using the stream values in the other definition."
  show TooMuchRecussion       = badUsage $ 
    "You have exceeded the limit of " ++ show maxRecursion ++ " recursive calls in a stream definition.  Likely, you have accidently defined a circular stream, such as 'x = x'.  Another possibility is you have defined a a polymorphic function with type constraints that references other streams.  For example,\n\n  nats :: (Typed a, Num a) => Stream a\n  nats = [0] ++ nats + 1\n\nis not allowed.  Make the definition monomorphic, or add a level of indirection, like \n\n  nats :: (Typed a, Num a) => Stream a\n  nats = n\n    where n = [0] ++ nats + 1\n\nFinally, you may have intended to generate a very large expression.  You can try shrinking the expression by using local variables.  It all else fails, you can increase the maximum size of ecursive calls by modifying 'maxRecursion' in copilot-language."
  show (DifferentTypes name) = badUsage $  
    "The external symbol " ++ name ++ " has been declared to have two different types!"
  show (Redeclared name) = badUsage $ 
    "The external symbol " ++ name ++ " has been redeclared to be a different symbol (e.g., a variable and an array, or a variable and a funciton symbol, etc.)."
  show (BadNumberOfArgs name) = badUsage $ 
    "The function symbol " ++ name ++ " has been redeclared to have different number of arguments."
  show (BadFunctionArgType name) = badUsage $ 
    "The funciton symbol " ++ name ++ " has been redeclared to an argument with different types."

instance Exception AnalyzeException

maxRecursion :: Int
maxRecursion = 100000

--------------------------------------------------------------------------------

type Env = Map ()

--------------------------------------------------------------------------------

analyze :: Spec -> IO ()
analyze spec = do
  refStreams <- newIORef M.empty
  mapM_ (analyzeTrigger  refStreams) (triggers  $ runSpec spec)
  mapM_ (analyzeObserver refStreams) (observers $ runSpec spec)
  specExts refStreams spec >>= analyzeExts 

--------------------------------------------------------------------------------

analyzeTrigger :: IORef Env -> Trigger -> IO ()
analyzeTrigger refStreams (Trigger _ e0 args) = 
  analyzeExpr refStreams e0 >> mapM_ analyzeTriggerArg args

  where
  analyzeTriggerArg :: Arg -> IO ()
  analyzeTriggerArg (Arg e) = analyzeExpr refStreams e

--------------------------------------------------------------------------------

analyzeObserver :: IORef Env -> Observer -> IO ()
analyzeObserver refStreams (Observer _ e) = analyzeExpr refStreams e

--------------------------------------------------------------------------------

data SeenExtern = NoExtern
                | SeenFun
                | SeenArr

--------------------------------------------------------------------------------

analyzeExpr :: IORef Env -> Stream a -> IO ()
analyzeExpr refStreams s = do
  b <- mapCheck refStreams
  when b (throw TooMuchRecussion)
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
      ExternFun _ args me -> 
        checkInterp >> checkArgs
        where
        checkInterp = case me of 
                        Nothing -> return ()
                        Just e  -> go seenExt nodes' e
        checkArgs = case seenExt of 
                      NoExtern -> mapM_ (\(Arg a) -> 
                                             go SeenFun nodes' a) args
                      SeenFun  -> throw NestedExternFun
                      SeenArr  -> throw NestedArray
      ExternArray _ idx _ _ -> case seenExt of 
                                 NoExtern -> go SeenArr nodes' idx
                                 SeenFun  -> throw NestedExternFun
                                 SeenArr  -> throw NestedArray
      Local e f           -> go seenExt nodes' e >> 
                             go seenExt nodes' (f (Var "dummy"))
      Var _               -> return ()
      Op1 _ e             -> go seenExt nodes' e
      Op2 _ e1 e2         -> go seenExt nodes' e1 >> 
                             go seenExt nodes' e2
      Op3 _ e1 e2 e3      -> go seenExt nodes' e1 >> 
                             go seenExt nodes' e2 >> 
                             go seenExt nodes' e3

--------------------------------------------------------------------------------

assertNotVisited :: Stream a -> DynStableName -> Env -> IO ()
assertNotVisited (Append _ _ _) _    _     = return ()
assertNotVisited _              dstn nodes =
  case M.lookup dstn nodes of
    Just () -> throw ReferentialCycle
    Nothing -> return ()

--------------------------------------------------------------------------------

mapCheck :: IORef Env -> IO Bool
mapCheck refStreams = do
  ref <- readIORef refStreams
  return $ getSize ref > maxRecursion

--------------------------------------------------------------------------------

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

-- An environment to store external variables, arrays, and functions, so that we
-- can check types in the expression---e.g., if we declare the same external to
-- have two different types.
data ExternEnv = ExternEnv
  { externVarEnv  :: [(String, C.SimpleType)]
  , externArrEnv  :: [(String, C.SimpleType)]
  , externFunEnv  :: [(String, C.SimpleType)] 
  , externFunArgs :: [(String, [C.SimpleType])] 
  }

--------------------------------------------------------------------------------

-- Make sure external variables, functions, and arrays are correctly typed.

analyzeExts :: ExternEnv -> IO ()
analyzeExts ExternEnv { externVarEnv  = vars
                      , externArrEnv  = arrs
                      , externFunEnv  = funs 
                      , externFunArgs = args }
    = do
    -- symbol names redeclared?
    findDups vars arrs
    findDups vars funs
    findDups arrs funs
    -- conflicting types?
    conflictingTypes vars
    conflictingTypes arrs
    conflictingTypes funs
    -- symbol names given different number of args and right types?
    funcArgCheck args
  
  where
  findDups :: [(String, C.SimpleType)] -> [(String, C.SimpleType)] -> IO ()
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

  groupByPred :: [(String, a)] -> [[(String, a)]]
  groupByPred = groupBy (\(n0,_) (n1,_) -> n0 == n1)

  foldCheck :: (String -> a -> a -> IO (String, a)) -> [(String, a)] -> IO ()
  foldCheck check grp = 
    foldM_ ( \(name, c0) (_, c1) -> check name c0 c1)
           (head grp) -- should be typesafe, since this is from groupBy
           grp

--------------------------------------------------------------------------------

specExts :: IORef Env -> Spec -> IO ExternEnv
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

collectExts :: C.Typed a => IORef Env -> Stream a -> ExternEnv -> IO ExternEnv
collectExts refStreams stream_ env_ = do
  b <- mapCheck refStreams
  when b (throw TooMuchRecussion)
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

      ExternFun name args me -> do
        env' <- case me of
                  Nothing -> return env 
                  Just e  -> go nodes env e
        env'' <- foldM (\env'' (Arg arg_) -> go nodes env'' arg_)
                   env' args 
        let argTypes = map (\(Arg arg_) -> getSimpleType arg_) args 
        let fun = (name, getSimpleType stream) 
        return env'' { externFunEnv  = fun : externFunEnv env''
                     , externFunArgs = (name, argTypes) : externFunArgs env''
                     }

      ExternArray name idx _ _ -> do
        env' <- go nodes env idx
        let arr = ( name, getSimpleType stream )
        return env' { externArrEnv = arr : externArrEnv env' }

      Local e _              -> go nodes env e 
      Var _                  -> return env
      Op1 _ e                -> go nodes env e 
      Op2 _ e1 e2            -> do env' <- go nodes env e1
                                   go nodes env' e2
      Op3 _ e1 e2 e3         -> do env' <- go nodes env e1  
                                   env'' <- go nodes env' e2 
                                   go nodes env'' e3 

--------------------------------------------------------------------------------

getSimpleType :: forall a. C.Typed a => Stream a -> C.SimpleType
getSimpleType _ = C.simpleType (C.typeOf :: C.Type a)

--------------------------------------------------------------------------------
