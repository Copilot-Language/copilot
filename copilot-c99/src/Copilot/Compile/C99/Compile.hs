{-# LANGUAGE GADTs #-}
-- | Compile Copilot specifications to C99 code.
module Copilot.Compile.C99.Compile
  ( compile
  , compileWith
  ) where

-- External imports
import           Data.List           ( nub, nubBy, union )
import           Data.Maybe          ( mapMaybe )
import           Data.Type.Equality  ( testEquality, (:~:)(Refl) )
import           Data.Typeable       ( Typeable )
import           Language.C99.Pretty ( pretty )
import qualified Language.C99.Simple as C
import           System.Directory    ( createDirectoryIfMissing )
import           System.Exit         ( exitFailure )
import           System.FilePath     ( (</>) )
import           System.IO           ( hPutStrLn, stderr )
import           Text.PrettyPrint    ( render )

-- Internal imports: Copilot
import Copilot.Core ( Expr (..), Spec (..), Stream (..), Struct (..),
                      Trigger (..), Type (..), UExpr (..), UType (..),
                      Value (..) )

-- Internal imports
import Copilot.Compile.C99.CodeGen        ( mkAccessDecln, mkBuffDecln,
                                            mkExtCpyDecln, mkExtDecln,
                                            mkGenFun, mkGenFunArray,
                                            mkIndexDecln, mkStep,
                                            mkStructDecln, mkStructForwDecln )
import Copilot.Compile.C99.External       ( External, gatherExts )
import Copilot.Compile.C99.Name           ( argNames, generatorName,
                                            generatorOutputArgName, guardName )
import Copilot.Compile.C99.Settings       ( CSettings,
                                            cSettingsOutputDirectory,
                                            cSettingsStepFunctionName,
                                            mkDefaultCSettings )
import Copilot.Compile.C99.Type           ( transType )
import Copilot.Compile.C99.Representation ( UniqueTrigger (..),
                                            mkUniqueTriggers )

-- | Compile a specification to a .h and a .c file.
--
-- The first argument is the settings for the C code generated.
--
-- The second argument is used as prefix for the .h and .c files generated.
compileWith :: CSettings -> String -> Spec -> IO ()
compileWith cSettings prefix spec
  | null (specTriggers spec)
  = do hPutStrLn stderr $
         "Copilot error: attempt at compiling empty specification.\n"
         ++ "You must define at least one trigger to generate C monitors."
       exitFailure

  | otherwise
  = do let cFile = render $ pretty $ C.translate $ compileC cSettings spec
           hFile = render $ pretty $ C.translate $ compileH cSettings spec
           typeDeclnsFile = safeCRender $ compileTypeDeclns cSettings spec

           cMacros = unlines [ "#include <stdint.h>"
                             , "#include <stdbool.h>"
                             , "#include <string.h>"
                             , "#include <stdlib.h>"
                             , "#include <math.h>"
                             , ""
                             , "#include \"" ++ prefix ++ "_types.h\""
                             , "#include \"" ++ prefix ++ ".h\""
                             , ""
                             ]

       let dir = cSettingsOutputDirectory cSettings
       createDirectoryIfMissing True dir
       writeFile (dir </> prefix ++ ".c") $ cMacros ++ cFile
       writeFile (dir </> prefix ++ ".h") hFile
       writeFile (dir </> prefix ++ "_types.h") typeDeclnsFile

-- | Compile a specification to a .h and a .c file.
--
-- The first argument is used as prefix for the .h and .c files generated.
compile :: String -> Spec -> IO ()
compile = compileWith mkDefaultCSettings

-- | Generate the .c file from a 'Spec'.
--
-- The generated C file has the following structure:
--
-- * Include .h file.
-- * Declarations of global buffers and indices.
-- * Generator functions for streams, guards and trigger arguments.
-- * Declaration of the @step()@ function.
compileC :: CSettings -> Spec -> C.TransUnit
compileC cSettings spec = C.TransUnit declns funs
  where
    declns =  mkExts exts
           ++ mkGlobals streams

    funs =  mkGenFuns streams uniqueTriggers
         ++ [mkStep cSettings streams uniqueTriggers exts]

    streams        = specStreams spec
    triggers       = specTriggers spec
    uniqueTriggers = mkUniqueTriggers triggers
    exts           = gatherExts streams triggers

    -- Make declarations for copies of external variables.
    mkExts :: [External] -> [C.Decln]
    mkExts = map mkExtCpyDecln

    -- Make buffer and index declarations for streams.
    mkGlobals :: [Stream] -> [C.Decln]
    mkGlobals streamList =  map buffDecln streamList
                         ++ map indexDecln streamList
      where
        buffDecln  (Stream sId buff _ ty) = mkBuffDecln  sId ty buff
        indexDecln (Stream sId _    _ _ ) = mkIndexDecln sId

    -- Make generator functions, including trigger arguments.
    mkGenFuns :: [Stream] -> [UniqueTrigger] -> [C.FunDef]
    mkGenFuns streamList triggerList =  map accessDecln streamList
                                     ++ map streamGen streamList
                                     ++ concatMap triggerGen triggerList
      where
        accessDecln :: Stream -> C.FunDef
        accessDecln (Stream sId buff _ ty) = mkAccessDecln sId ty buff

        streamGen :: Stream -> C.FunDef
        streamGen (Stream sId _ expr ty) =
          exprGen (generatorName sId) (generatorOutputArgName sId) expr ty

        triggerGen :: UniqueTrigger -> [C.FunDef]
        triggerGen (UniqueTrigger uniqueName (Trigger _name guard args)) = guardDef : argDefs
          where
            guardDef = mkGenFun (guardName uniqueName) guard Bool
            argDefs  = zipWith argGen (argNames uniqueName) args

            argGen :: String -> UExpr -> C.FunDef
            argGen argName (UExpr ty expr) =
              exprGen argName (argName ++ "_output") expr ty

        -- Create a function that calculates the current value generated by an
        -- expression `expr` of type `ty`. The generator treats arrays
        -- specially, and the function takes an output array as a parameter.
        -- The second identifier `outputArrName` is not used if `expr` is not an
        -- array.
        exprGen :: C.Ident -> C.Ident -> Expr a -> Type a -> C.FunDef
        exprGen funName outputArrName expr ty@(Array _) =
          mkGenFunArray funName outputArrName expr ty
        exprGen funName _ expr ty =
          mkGenFun funName expr ty

-- | Generate the .h file from a 'Spec'.
compileH :: CSettings -> Spec -> C.TransUnit
compileH cSettings spec = C.TransUnit declns []
  where
    declns =  mkStructForwDeclns exprs
           ++ mkExts exts
           ++ extFunDeclns triggers
           ++ [stepDecln]

    exprs    = gatherExprs streams triggers
    exts     = gatherExts streams triggers
    streams  = specStreams spec

    -- Remove duplicates due to multiple guards for the same trigger.
    triggers = nubBy compareTrigger (specTriggers spec)

    mkStructForwDeclns :: [UExpr] -> [C.Decln]
    mkStructForwDeclns es = mapMaybe mkDecln uTypes
      where
        mkDecln (UType ty) = case ty of
          Struct _ -> Just $ mkStructForwDecln ty
          _        -> Nothing

        uTypes = nub $ concatMap (\(UExpr _ e) -> exprTypes e) es

    -- Make declarations for external variables.
    mkExts :: [External] -> [C.Decln]
    mkExts = map mkExtDecln

    extFunDeclns :: [Trigger] -> [C.Decln]
    extFunDeclns = map extFunDecln
      where
        extFunDecln :: Trigger -> C.Decln
        extFunDecln (Trigger name _ args) = C.FunDecln Nothing cTy name params
          where
            cTy    = C.TypeSpec C.Void
            params = zipWith mkParam (argNames name) args

            mkParam paramName (UExpr ty _) = C.Param (mkParamTy ty) paramName

            -- Special case for Struct, to pass struct arguments by reference.
            -- Arrays are also passed by reference, but using C's array type
            -- does that automatically.
            mkParamTy ty =
              case ty of
                Struct _ -> C.Ptr (transType ty)
                _        -> transType ty

    -- Declaration for the step function.
    stepDecln :: C.Decln
    stepDecln = C.FunDecln Nothing (C.TypeSpec C.Void)
                    (cSettingsStepFunctionName cSettings) []

-- | Generate a C translation unit that contains all type declarations needed
-- by the Copilot specification.
compileTypeDeclns :: CSettings -> Spec -> C.TransUnit
compileTypeDeclns _cSettings spec = C.TransUnit declns []
  where
    declns = mkTypeDeclns exprs

    exprs    = gatherExprs streams triggers
    streams  = specStreams spec
    triggers = specTriggers spec

    -- Generate type declarations.
    mkTypeDeclns :: [UExpr] -> [C.Decln]
    mkTypeDeclns es = mapMaybe mkTypeDecln uTypes
      where
        uTypes = nub $ concatMap (\(UExpr _ e) -> exprTypes e) es

        mkTypeDecln (UType ty) = case ty of
          Struct _ -> Just $ mkStructDecln ty
          _        -> Nothing

-- * Auxiliary definitions

-- | Render a C.TransUnit to a String, accounting for the case in which the
-- translation unit is empty.
safeCRender :: C.TransUnit -> String
safeCRender (C.TransUnit [] []) = ""
safeCRender transUnit           = render $ pretty $ C.translate transUnit

-- ** Obtain information from Copilot Core Exprs and Types.

-- | List all types of an expression, returns items uniquely.
exprTypes :: Typeable a => Expr a -> [UType]
exprTypes e = case e of
  Const ty _            -> typeTypes ty
  Local ty1 ty2 _ e1 e2 -> typeTypes ty1 `union` typeTypes ty2
                             `union` exprTypes e1 `union` exprTypes e2
  Var ty _              -> typeTypes ty
  Drop ty _ _           -> typeTypes ty
  ExternVar ty _ _      -> typeTypes ty
  Op1 _ e1              -> exprTypes e1
  Op2 _ e1 e2           -> exprTypes e1 `union` exprTypes e2
  Op3 _ e1 e2 e3        -> exprTypes e1 `union` exprTypes e2
                             `union` exprTypes e3
  Label ty _ _          -> typeTypes ty

-- | List all types of a type, returns items uniquely.
typeTypes :: Typeable a => Type a -> [UType]
typeTypes ty = case ty of
  Array ty' -> typeTypes ty' `union` [UType ty]
  Struct x  -> concatMap (\(Value ty' _) -> typeTypes ty') (toValues x)
                 `union` [UType ty]
  _         -> [UType ty]

-- | Collect all expression of a list of streams and triggers and wrap them
-- into an UEXpr.
gatherExprs :: [Stream] -> [Trigger] -> [UExpr]
gatherExprs streams triggers =  map streamUExpr streams
                             ++ concatMap triggerUExpr triggers
  where
    streamUExpr  (Stream _ _ expr ty)   = UExpr ty expr
    triggerUExpr (Trigger _ guard args) = UExpr Bool guard : args

-- | We consider triggers to be equal, if their names match and the number and
-- types of arguments.
compareTrigger :: Trigger -> Trigger -> Bool
compareTrigger (Trigger name1 _ args1) (Trigger name2 _ args2)
  = name1 == name2 && compareArguments args1 args2

  where
    compareArguments :: [UExpr] -> [UExpr] -> Bool
    compareArguments []     []     = True
    compareArguments []     _      = False
    compareArguments _      []     = False
    compareArguments (x:xs) (y:ys) = compareUExpr x y && compareArguments xs ys

    compareUExpr :: UExpr -> UExpr -> Bool
    compareUExpr (UExpr ty1 _) (UExpr ty2 _)
      | Just Refl <- testEquality ty1 ty2 = True
      | otherwise                         = False
