{-# OPTIONS_GHC -XRelaxedPolyRec #-}

-- | This module provides a way to check that a /Copilot/ specification is compilable
module Language.Copilot.Analyser(
        -- * Main error checking functions
        check, Error(..), SpecSet(..),
        -- * Varied other things
        getExternalVars, getAtomType
        {-
        -- * Dependency Graphs (experimental)
        Weight, Node(..), DependencyGraph,
        mkDepGraph, showDG -}
    ) where
    
import Language.Copilot.Core

import qualified Language.Atom as A

import Data.List

type Weight = Int

-- | Used for representing an error in the specification, detected by @'check'@
data Error = 
      BadSyntax String Var -- ^ the BNF is not respected
    | BadDrop Int Var -- ^ A drop expression of less than 0 is used
    | BadSamplingPhase Var Var Phase -- ^ if an external variable is sampled at phase 0 then there is no time for the stream to be updated
    | BadType Var Var -- ^ either a variable is not defined, or not with the good type ; there is no implicit conversion of types in /Copilot/
    | NonNegativeWeightedClosedPath [Var] Weight -- ^ The algorithm to compile /Copilot/ specification can only work if there is no negative weighted closed path in the specification, as described in the original research paper
    | DependsOnClosePast [Var] Var Weight Weight -- ^ Could be compiled, but would need bigger prophecyArrays
    | DependsOnFuture [Var] Var Weight-- ^ If an output depends of a future of an input it will be hard to compile to say the least

instance Show Error where
    show (BadSyntax s v) = 
        "Error syntax : " ++ s ++ "is not allowed in that position in stream " ++ v ++ "\n"
    show (BadDrop i v) = 
        "Error : a Drop in stream " ++ v ++ "drops the number " ++ show i ++ 
        "of elements. " ++ show i ++ " is negative, and Drop only accepts positive arguments. \n"
    show (BadSamplingPhase v v' ph) = 
        "Error : the external variable " ++ v' ++ " is sampled at phase " ++ show ph ++ 
            " in the stream" ++ v ++ ". Sampling can only occur from phase 1 onwards. \n"
    show (BadType v v') =
        "Error : the monitor variable " ++ v ++ ", called in the stream " ++ v' ++
            " either does not exist, or don't have the right type (there is no implicit conversion)\n"
    show (NonNegativeWeightedClosedPath vs w) =
        "Error : the following path is closed in the dependency graph of this "
            ++ "specification and have weight " ++ show w ++ " which is positive (append decrease the weight, "
            ++ "while drop increase it). This is forbidden to avoid streams which could "
            ++ "take 0 or several different values.  Try adding some initial elements (e.g., [0,0,0] ++ ...) "
            ++ "to the offending streams. \n"
            ++ "Path : " ++ show (reverse vs) ++ "\n"
    show (DependsOnClosePast vs v w len) =
        "Error : the following path is of weight " ++ show w ++ " ending in "
            ++ "the external variable " ++ v ++ " while the first variable of that path "
            ++ "has a prophecy array of length " ++ show len ++ ", which is strictly greater "
            ++ "than the weight. This is forbidden. \n"
            ++ "Path : " ++ show (reverse vs) ++ "\n" 
    show (DependsOnFuture vs v w) =
        "Error : the following path is of weight " ++ show w ++ " which is strictly positive. "
            ++ "This means that the first variable depends on the future of the external variable "
            ++ v ++ " which is quoted in the last variable of the path. This is obviously impossible. \n"
            ++ "Path : " ++ show (reverse vs) ++ "\n"

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
-- Else, returns @Nothing@
check :: StreamableMaps Spec -> Maybe Error
check streams = 
    syntaxCheck streams &&> defCheck streams

-- Represents all the kind of specs that are authorized after a given operator
data SpecSet = AllSpecSet | FunSpecSet | DropSpecSet deriving Eq

-- Check that the AST of the copilot specification match the BNF
-- Could have been verified by the type checker if the type of Spec had been cut
-- But then there would have been quite a lot construction/deconstruction to do everywhere.
-- Hence the compact type for Spec and this extra check.
syntaxCheck :: StreamableMaps Spec -> Maybe Error
syntaxCheck streams =
    foldStreamableMaps (checkSyntaxSpec AllSpecSet) streams Nothing
    where
        checkSyntaxSpec :: Streamable a => SpecSet -> Var -> Spec a -> Maybe Error -> Maybe Error
        checkSyntaxSpec set v s e =
            e &&>
                case s of
                    PVar _ v' ph -> ph > 0 ||> BadSamplingPhase v v' ph
                    Var _ -> Nothing
                    Const _ -> Nothing
                    F _ _ s0 -> set /= DropSpecSet ||> BadSyntax "F" v &&>
                        (checkSyntaxSpec FunSpecSet v s0 Nothing)
                    F2 _ _ s0 s1 -> set /= DropSpecSet ||> BadSyntax "F2" v &&>
                        (checkSyntaxSpec FunSpecSet v s0 Nothing) &&>
                        checkSyntaxSpec FunSpecSet v s1 Nothing
                    F3 _ _ s0 s1 s2 -> set /= DropSpecSet ||> BadSyntax "F3" v &&>
                        (checkSyntaxSpec FunSpecSet v s0 Nothing) &&> 
                        (checkSyntaxSpec FunSpecSet v s1 Nothing) &&>
                        checkSyntaxSpec FunSpecSet v s2 Nothing 
                    Append _ s0 -> set == AllSpecSet ||> BadSyntax "Append" v &&> 
                        checkSyntaxSpec AllSpecSet v s0 Nothing
                    Drop i s0 -> (0 <= i) ||> BadDrop i v &&> 
                                 (checkSyntaxSpec DropSpecSet v s0 Nothing)

-- checks that streams are well defined (ie can be compiled)
-- Currently very inefficient (for simplicity's sake), 
-- could probably be optimized if need be
-- by keeping weights of paths in a matrix and doing some linear algebra
-- (fast exponentiation could give some nice results)
-- could also reuse the dependency graph (see below)
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
                        PVar t v _ -> case () of
                                () | n > 0 -> Just $ DependsOnFuture vs v n
                                () | n > negate (prophecyArrayLength s0) -> Just $ DependsOnClosePast vs v n (prophecyArrayLength s0)
                                () | t /= getAtomType s -> Just $ BadType v (head vs)
                                _ -> Nothing
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
                        F3 _ _ s1 s2 s3 -> checkPath n vs s1 &&> checkPath n vs s2 &&> checkPath n vs s3
                        Append l s' -> checkPath (n - length l) vs s'
                        Drop i s' -> checkPath (n + i) vs s'
    in
    foldStreamableMaps checkPathsFromSpec streams Nothing

getAtomType :: Streamable a => Spec a -> A.Type
getAtomType s =
  let unitElem = unit
      _ = (Const unitElem) `asTypeOf` s -- to help the typechecker
  in atomType unitElem

getExternalVars :: StreamableMaps Spec -> [(A.Type, Var, Phase)]
getExternalVars streams =
    nub $ foldStreamableMaps decl streams []
    where
        decl :: Streamable a => Var -> Spec a -> [(A.Type, Var, Phase)] -> [(A.Type, Var, Phase)]
        decl _ s ls =
            case s of
                PVar t v ph -> (t, v, ph) : ls
                F _ _ s0 -> decl undefined s0 ls
                F2 _ _ s0 s1 -> decl undefined s0 $ decl undefined s1 ls
                F3 _ _ s0 s1 s2 -> decl undefined s0 $ decl undefined s1 $ decl undefined s2 ls
                Drop _ s' -> decl undefined s' ls
                Append _ s' -> decl undefined s' ls
                _ -> ls

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
