{-# LANGUAGE RelaxedPolyRec #-}

-- | This module provides a way to check that a /Copilot/ specification is
-- compilable
module Language.Copilot.Analyser(
        -- * Main error checking functions
        check, Error(..), SpecSet(..),
        -- * Varied other things
        getExternalVars
        {-
        -- * Dependency Graphs (experimental)
        Weight, Node(..), DependencyGraph,
        mkDepGraph, showDG -}
    ) where

import Language.Copilot.Core

import Data.List
import Data.Maybe(mapMaybe)

type Weight = Int

-- | Used for representing an error in the specification, detected by @'check'@
data Error =
      BadSyntax String Var -- ^ the BNF is not respected
    | BadDrop Int Var -- ^ A drop expression of less than 0 is used
    | BadPArrSpec Var Ext String -- ^ External array indexes can only take
                                 -- variables or constants as indexes.
    | BadType Var String -- ^ either a variable is not defined, or not with the
                         -- good type ; there is no implicit conversion of types
                         -- in /Copilot/.
    | BadTypeExt Ext Var -- ^ either an external variable is not defined, or not
                         -- with the good type; there is no implicit conversion
                         -- of types in /Copilot/
    | NonNegativeWeightedClosedPath [Var] Weight -- ^ The algorithm to compile
                                                 -- /Copilot/ specification can
                                                 -- only work if there is no
                                                 -- negative weighted closed
                                                 -- path in the specification,
                                                 -- as described in the original
                                                 -- research paper
    | DependsOnClosePast [Var] Ext Weight Weight -- ^ Could be compiled, but
                                                 -- would need bigger
                                                 -- prophecyArrays
    | DependsOnFuture [Var] Ext Weight -- ^ If an output depends of a future of
                                       -- an input it will be hard to compile to
                                       -- say the least
    | NoInit Var -- ^ If we are sampling a function and it has an
                 -- argument that is a Copilot stream, the weight of
                 -- that stream must have at least one initial element.

instance Show Error where
    show (BadSyntax s v) =
       unlines 
        ["Error syntax : " ++ s ++ " is not allowed in that position in stream " 
         ++ v ++ "."]
    show (BadDrop i v) =
       unlines
        [ "Error : a Drop in stream " ++ v ++ " drops the number " ++ show i ++
          "of elements.\n" 
        , show i ++ " is negative, and Drop only accepts positive arguments.\n"
        ]
    show (BadPArrSpec v arr idx) = 
       unlines
        [ "Error : the index into an external array can only take a "
            ++ "variable or a constant.  The index\n"  
        ,   idx ++ "\n\n in external array "
            ++ show arr ++ "in the definition of stream " ++ v ++ " is not of that "
            ++ "form.\n"
        ]
    show (BadType v v') =
       unlines
        [ "Error : the monitor variable " ++ v ++ ", called from " 
          ++ v' ++ ", either"
        , "does not exist, or don't have the right type (there is no implicit " 
          ++ "conversion).\n"
        ]
    show (BadTypeExt v v') =
       unlines
        [ "Error : the external call " ++ show v ++ ", called in the stream " 
          ++ v' ++ ", either"
        , "does not exist, or don't have the right type (there is no implicit " 
          ++ "conversion).\n"
        ]
    show (NonNegativeWeightedClosedPath vs w) =
       unlines 
        [ "Error : the following path is closed in the dependency graph of the "
            ++ "specification and has" 
        , "weight " ++ show w ++ " which is positive (append decreases the weight, "
          ++ "while drop increases it)."  
        , "This is forbidden to avoid streams which could take 0 or several different"
          ++ " values."
        , "Try adding some initial elements (e.g., [0,0,0] ++ ...) "
            ++ "to the offending streams."
        , "Path : " ++ show (reverse vs) ++ "\n"
        ]
    show (DependsOnClosePast vs v w len) =
       unlines
        [ "Error : the following path is of weight " ++ show w ++ " ending in "
          ++ "the external variable " ++ show v 
        , "while the first variable of that path has a prophecy array of length " 
          ++ show len ++ "," 
        , "which is strictly greater than the weight. This is forbidden."
        , "Path : " ++ show (reverse vs) ++ "\n"
        ]
    show (DependsOnFuture vs v w) =
       unlines
        [ "Error : the following path is of weight " ++ show w 
          ++ " which is strictly positive."
        , "This means that the first variable depends on the future of the "
          ++ "external variable " ++ show v 
        , "which is quoted in the last variable of the path.  This is "
          ++ "obviously impossible."
        , "Path : " ++ show (reverse vs) ++ "\n"
        ]
    show (NoInit v) = 
          "Error : the Copilot variable " ++ v ++ " appears either as an argument in an "
          ++ "external function that is sampled or an index in an external array being sampled. "
          ++ "In either case, the stream must have an initial value (that is not drawn from an "
          ++ "external variable)."

(&&>) :: Maybe a -> Maybe a -> Maybe a
m &&> m' =
    case m of
        Just _ -> m
        Nothing -> m'

(||>) :: Bool -> a -> Maybe a
b ||> x =
    if b
        then Nothing
        else Just x

infixr 2 ||>
infixr 1 &&>

-- | Check a /Copilot/ specification.
-- If it is not compilable, then returns an error describing the issue.
-- Else, returns @Nothing@.
check :: StreamableMaps Spec -> Maybe Error
check streams =
  syntaxCheck streams &&> defCheck streams &&> checkInitsArgs streams

-- Represents all the kind of specs that are authorized after a given operator.
data SpecSet = AllSpecSet | FunSpecSet | DropSpecSet 
  deriving Eq

-- Check that the AST of the copilot specification match the BNF could have been
-- verified by the type checker if the type of Spec had been cut But then there
-- would have been quite a lot construction/deconstruction to do everywhere.
-- Hence the compact type for Spec and this extra check.
syntaxCheck :: StreamableMaps Spec -> Maybe Error
syntaxCheck streams =
    foldStreamableMaps (checkSyntaxSpec AllSpecSet) streams Nothing
    where
        checkSyntaxSpec :: Streamable a 
                        => SpecSet -> Var -> Spec a -> Maybe Error -> Maybe Error
        checkSyntaxSpec set v s e =
            e &&>
                case s of
                    Var _ -> Nothing
                    Const _ -> Nothing
                    PVar _ _ -> Nothing -- checkSampling s0
                    PArr _ (arr,s0) -> checkIndex v arr s0 
-- case chkInitElem streams (getElem v streams `asTypeOf` s0) of
--                                       Left ()  -> Just $ NoInit (show arr) v
--                                       Right () -> Nothing
--                          _       -> Just $ BadArrIdx (show arr) (show s0)
--                      ) &&> checkSampling arr
--                        &&> checkSyntaxSpec PArrSet v s0 Nothing                      
                    -- PVar _ _ -> Nothing 
                    -- PArr _ (arr,s0) -> 
                    F _ _ s0 -> set /= DropSpecSet ||> BadSyntax "F" v 
                      &&> checkSyntaxSpec FunSpecSet v s0 Nothing
                    F2 _ _ s0 s1 -> set /= DropSpecSet ||> BadSyntax "F2" v 
                      &&> checkSyntaxSpec FunSpecSet v s0 Nothing
                      &&> checkSyntaxSpec FunSpecSet v s1 Nothing
                    F3 _ _ s0 s1 s2 -> set /= DropSpecSet ||> BadSyntax "F3" v 
                      &&> checkSyntaxSpec FunSpecSet v s0 Nothing
                      &&> checkSyntaxSpec FunSpecSet v s1 Nothing
                      &&> checkSyntaxSpec FunSpecSet v s2 Nothing
                    Append _ s0 -> set == AllSpecSet ||> BadSyntax "Append" v 
                      &&> checkSyntaxSpec AllSpecSet v s0 Nothing
                    Drop i s0 -> (0 <= i) ||> BadDrop i v 
                      &&> checkSyntaxSpec DropSpecSet v s0 Nothing
        checkIndex _ _ (Var _)      = Nothing
        checkIndex _ _ (Const _)    = Nothing
        checkIndex v arr idx = Just $ BadPArrSpec v arr (show idx)
        -- checkSampling s = 
        --   case s of
        --     ExtV _   -> Nothing
        --     fun@(Fun f args) ->  
        --       foldl (\n arg -> case arg of
        --                          C _ -> n &&> Nothing
        --                                 -- already check elems defined in defCheck
        --                          V v -> n &&> (case chkInitElem streams (getElem v streams) of 
        --                                          Left ()  -> Just $ NoInit (show fun) v
        --                                          Right () -> Nothing
        --                                       )
        --             ) Nothing args


-- | Checks that streams are well defined (i.e., can be compiled).
defCheck :: StreamableMaps Spec -> Maybe Error
defCheck streams =
    let checkPathsFromSpec :: Streamable a => Var -> Spec a -> Maybe Error -> Maybe Error
        checkPathsFromSpec v0 s0 e =
            e &&> checkPath 0 [v0] s0
            where
                prophecyArrayLength s =
                    case s of
                        Append ls s' -> length ls + prophecyArrayLength s'
                        _ -> 0
                checkPath :: Streamable a => Int -> [Var] -> Spec a -> Maybe Error
                checkPath n vs s =
                    case s of
                        PVar t v -> case () of
                                () | n > 0 -> Just $ DependsOnFuture vs v n
                                   | n > negate (prophecyArrayLength s0) -> 
                                           Just $ DependsOnClosePast vs v n 
                                                    (prophecyArrayLength s0)
                                   | t /= getAtomType s -> 
                                            Just $ BadTypeExt v (head vs)
                                _ -> Nothing
                        PArr t (arr, idx) 
                                   | n > 0 -> Just $ DependsOnFuture vs arr n
                                   | n > negate (prophecyArrayLength idx) -> 
                                           Just $ DependsOnClosePast vs arr n 
                                                    (prophecyArrayLength idx)
                                   | t /= getAtomType s -> 
                                       Just $ BadTypeExt arr (head vs)
                                   | otherwise -> 
                                       case idx of
                                         Const _ -> checkPath n vs idx
                                         Var _   -> checkPath n vs idx
                                         _       -> 
                                           Just $ BadPArrSpec v0 arr (show idx)
                        Var v -> 
                            if elem v vs
                                then if n >= 0
                                    then Just $ NonNegativeWeightedClosedPath vs n
                                    else Nothing
                                else
                                    let spec = getMaybeElem v streams in
                                    case spec of
                                        Nothing -> Just $ BadType v (head vs)
                                        Just s' -> checkPath n (v:vs) (s' `asTypeOf` s)
                        Const _ -> Nothing
                        F _ _ s1 -> checkPath n vs s1
                        F2 _ _ s1 s2 -> checkPath n vs s1 &&> checkPath n vs s2
                        F3 _ _ s1 s2 s3 -> checkPath n vs s1 &&> checkPath n vs s2 
                                             &&> checkPath n vs s3
                        Append l s' -> checkPath (n - length l) vs s'
                        Drop i s' -> checkPath (n + i) vs s'
    in foldStreamableMaps checkPathsFromSpec streams Nothing

getExternalVars :: StreamableMaps Spec -> [Exs]
getExternalVars streams =
    nub $ foldStreamableMaps decl streams []
    where
        decl :: Streamable a => Var -> Spec a -> [Exs] -> [Exs]
        decl _ s ls =
            case s of
                PVar t v -> (t, v, ExtRetV) : ls
                PArr t (arr, s0) -> (t, arr
                                    , ExtRetA (case s0 of
                                                 Var v -> V v
                                                 Const c -> C (show c)
                                                 _ -> error "Error in getExternalVars.")
                                    ) : ls
                F _ _ s0 -> decl undefined s0 ls
                F2 _ _ s0 s1 -> decl undefined s0 $ decl undefined s1 ls
                F3 _ _ s0 s1 s2 -> decl undefined s0 $ decl undefined s1 
                                     $ decl undefined s2 ls
                Drop _ s' -> decl undefined s' ls
                Append _ s' -> decl undefined s' ls
                _ -> ls

-- | Is there an initial element (for sampling functions)?
checkInitsArgs :: StreamableMaps Spec -> Maybe Error
checkInitsArgs streams =
  let checkInits :: Streamable a => Var -> Spec a -> Maybe Error -> Maybe Error
      checkInits v s err =
        err &&> if checkPath 0 [v] s >= 0 then Just (NoInit v)
                  else Nothing
        where checkPath :: Streamable a => Int -> [Var] -> Spec a -> Int
              checkPath n vs s0 = 
                if (v `elem` samplingFuncs) || (v `elem` arrIndices)
                  then case s0 of
                         Var v0    -> if elem v0 vs then n
                                       else checkPath n (v0:vs) 
                                              (getElem v0 streams `asTypeOf` s0)
                         Append ls s1 -> checkPath (n - length ls) vs s1
                         Drop m s1    -> checkPath (m + n) vs s1
                         _ -> n
                         -- Const _  -> n
                         -- PVar _ _ -> n
                         -- PArr _ _ -> n
                         -- F _ _ _      -> n
                         -- F2 _ _ _ _   -> n
                         -- F3 _ _ _ _ _ -> n
                  else (-1)
              samplingFuncs = 
                concatMap (\x -> case x of 
                                   (_, Fun _ args, _) -> 
                                     mapMaybe (\arg -> 
                                       case arg of
                                         C _ -> Nothing
                                         V v0 -> Just v0
                                              ) args
                                   (_, ExtV _, _) -> [])
                (getExternalVars streams)
              arrIndices = mapMaybe (\x -> case x of
                                             (_,_,ExtRetV) -> Nothing 
                                             (_,_,ExtRetA idx) -> 
                                               case idx of
                                                 V v' -> Just v'
                                                 C _ -> Nothing)
                           (getExternalVars streams)
  in foldStreamableMaps checkInits streams Nothing

-- chkInitElem :: Streamable a => StreamableMaps Spec -> Spec a -> Either () ()
-- chkInitElem streams spec = 
--   initElem spec 0 []
--   where 
--     -- getSpec :: Streamable a => Var -> Spec a
--     -- getSpec v = let s = getMaybeElem v streams in
--     --                case s of
--     --                  Nothing -> error $ "Error: No Copilot stream "
--     --                             ++ v ++ " exists."
--     --                  Just s0 -> s0 `asTypeOf` s
--     initElem :: Streamable a => Spec a -> Int -> [Var] -> Either () ()
--     initElem s cnt vs =
--       case s of
--         Var v    -> if elem v vs then initChk 
--                       else initElem (getElem v streams) cnt (v:vs)
--         Const _  -> initChk
--         PVar t v -> initChk
--         PArr t (arr, s0) -> initChk
--         F _ _ _      -> initChk
--         F2 _ _ _ _   -> initChk
--         F3 _ _ _ _ _ -> initChk
--         Append ls s0 -> initElem s0 (cnt - length ls) vs
--         Drop n s0    -> initElem s0 (cnt + n) vs
--       where initChk = if cnt < 0 then Right () else Left ()

---- Dependency graphs (for next version of nNWCP, and for scheduling)
{-
type Weight = Int
data Node =
    InternalVar Var [(Weight, Node)]
    | ExternalVar Var Phase
    deriving Show -- for debug

instance Eq Node where
    InternalVar v _ == InternalVar v' _ = v == v'
    ExternalVar v ph == ExternalVar v' ph' = v == v' && ph == ph'
    _ == _ = False

type DependencyGraph = [Node]

showDG :: DependencyGraph -> [String]
showDG dG = map show dG

mkDepGraph :: StreamableMaps Spec -> DependencyGraph
mkDepGraph streams =
    dGFixpoint
    where
        dGFixpoint :: DependencyGraph
        dGFixpoint = foldStreamableMaps mkNode streams []
        mkNode :: Streamable a => Var -> Spec a -> DependencyGraph -> DependencyGraph
        mkNode v s dG =
            let edges = mkEdges 0 s
                externalNodes = mkExternalNodes s in
            (InternalVar v edges) : (nub $ externalNodes ++ dG)
        -- TODO : the following functions can probably be fused together
        mkExternalNodes :: Spec a -> [Node]
        mkExternalNodes s =
            case s of
                PVar _ v ph -> [ExternalVar v ph]
                Var _ -> []
                Const _ -> []
                F _ _ s0 -> mkExternalNodes s0
                F2 _ _ s0 s1 -> mkExternalNodes s0 ++ mkExternalNodes s1
                F3 _ _ s0 s1 s2 -> mkExternalNodes s0 ++ mkExternalNodes s1 ++ mkExternalNodes s2
                Append _ s0 -> mkExternalNodes s0
                Drop _ s0 -> mkExternalNodes s0
        mkEdges :: Weight -> Spec a -> [(Weight, Node)]
        mkEdges w s =
            case s of
                PVar _ v ph -> [(w, getNode v $ Just ph)]
                Var v -> [(w, getNode v Nothing)]
                Const _ -> []
                F _ _ s0 -> mkEdges w s0
                F2 _ _ s0 s1 -> mkEdges w s0 ++ mkEdges w s1
                F3 _ _ s0 s1 s2 -> mkEdges w s0 ++ mkEdges w s1 ++ mkEdges w s2
                Append ls s0 -> mkEdges (w - length ls) s0
                Drop i s0 -> mkEdges (w + i) s0
        getNode :: Var -> Maybe Phase -> Node
        getNode v mp =
             case mp of
                Nothing -> fromJust $ find ((==) (InternalVar v [])) dGFixpoint
                Just ph -> fromJust $ find ((==) (ExternalVar v ph)) dGFixpoint -}
