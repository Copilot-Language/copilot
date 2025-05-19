{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compile Copilot specifications to Bluespec code.
module Copilot.Compile.Bluespec.Compile
  ( compile
  , compileWith
  ) where

-- External imports
import Data.List                      (nub, nubBy, union)
import Data.Maybe                     (catMaybes, maybeToList)
import Data.String                    (IsString (..))
import Data.Type.Equality             (testEquality, (:~:)(Refl))
import Data.Typeable                  (Typeable)
import qualified Language.Bluespec.Classic.AST as BS
import qualified Language.Bluespec.Classic.AST.Builtin.Ids as BS
import qualified Language.Bluespec.Classic.AST.Builtin.Types as BS
import Text.PrettyPrint.HughesPJClass (Pretty (..), render)
import System.Directory               (createDirectoryIfMissing)
import System.Exit                    (exitFailure)
import System.FilePath                ((</>))
import System.IO                      (hPutStrLn, stderr)

-- Internal imports: Copilot
import Copilot.Core

-- Internal imports
import Copilot.Compile.Bluespec.CodeGen
import Copilot.Compile.Bluespec.External
import Copilot.Compile.Bluespec.FloatingPoint
import Copilot.Compile.Bluespec.Name
import Copilot.Compile.Bluespec.Representation
import Copilot.Compile.Bluespec.Settings

-- | Compile a specification to a Bluespec file.
--
-- The first argument is the settings for the Bluespec code generated.
--
-- The second argument is used as a module name and the prefix for the .bs files
-- that are generated.
compileWith :: BluespecSettings -> String -> Spec -> IO ()
compileWith bsSettings prefix spec
  | null triggers
  = do hPutStrLn stderr $
         "Copilot error: attempt at compiling empty specification.\n"
         ++ "You must define at least one trigger to generate Bluespec monitors."
       exitFailure

  | incompatibleTriggers triggers
  = do hPutStrLn stderr $
         "Copilot error: attempt at compiling specification with conflicting "
         ++ "trigger definitions.\n"
         ++ "Multiple triggers have the same name, but different argument "
         ++ "types.\n"
       exitFailure

  | otherwise
  = do let typesBsFile = render $ pPrint $ compileTypesBS bsSettings prefix spec
           ifcBsFile   = render $ pPrint $ compileIfcBS   bsSettings prefix spec
           bsFile      = render $ pPrint $ compileBS      bsSettings prefix spec

       let dir = bluespecSettingsOutputDirectory bsSettings
       createDirectoryIfMissing True dir
       writeFile (dir </> specTypesPkgName prefix ++ ".bs") typesBsFile
       writeFile (dir </> specIfcPkgName prefix ++ ".bs") ifcBsFile
       writeFile (dir </> "bs_fp.c") copilotBluespecFloatingPointC
       writeFile (dir </> "BluespecFP.bsv") copilotBluespecFloatingPointBSV
       writeFile (dir </> prefix ++ ".bs") bsFile
  where
    triggers = specTriggers spec

    -- Check that two triggers do no conflict, that is: if their names are
    -- equal, the types of their arguments should be equal as well.
    incompatibleTriggers :: [Trigger] -> Bool
    incompatibleTriggers = pairwiseAny conflict
      where
        conflict :: Trigger -> Trigger -> Bool
        conflict t1@(Trigger name1 _ _) t2@(Trigger name2 _ _) =
          name1 == name2 && not (compareTrigger t1 t2)

        -- True if the function holds for any pair of elements. We assume that
        -- the function is commutative.
        pairwiseAny :: (a -> a -> Bool) -> [a] -> Bool
        pairwiseAny _ []     = False
        pairwiseAny _ (_:[]) = False
        pairwiseAny f (x:xs) = any (f x) xs || pairwiseAny f xs

-- | Compile a specification to a Bluespec.
--
-- The first argument is used as a prefix for the generated .bs files.
compile :: String -> Spec -> IO ()
compile = compileWith mkDefaultBluespecSettings

-- | Generate a @<prefix>.bs@ file from a 'Spec'. This is the main payload of
-- the Bluespec backend.
--
-- The generated Bluespec file will import a handful of files from the standard
-- library, as well as the following generated files:
--
-- * @<prefix>Ifc.bs@, which defines the interface containing the trigger
--   functions and external variables.
--
-- * @<prefix>Types.bs@, which defines any structs used in the 'Spec'.
--
-- It will also generate a @mk<prefix> :: Module <prefix>Ifc -> Module Empty@
-- function, which defines the module structure for this 'Spec'. The
-- @mk<prefix>@ function has the following structure:
--
-- * First, bind the argument of type @Module <prefix>Ifc@ so that trigger
--   functions can be invoked and external variables can be used.
--
-- * Next, declare stream buffers and indices.
--
-- * Next, declare generator functions for streams, accessor functions for
--   streams, and guard functions for triggers.
--
-- * Next, declare rules for each trigger function.
--
-- * Finally, declare a single rule that updates the stream buffers and
--   indices.
compileBS :: BluespecSettings -> String -> Spec -> BS.CPackage
compileBS _bsSettings prefix spec =
    BS.CPackage
      (BS.mkId BS.NoPos (fromString prefix))
      (Right [])
      (stdLibImports ++ genImports)
      []
      [moduleDef]
      []
  where
    -- import <prefix>Types
    -- import <prefix>Ifc
    genImports :: [BS.CImport]
    genImports =
      [ BS.CImpId False $ BS.mkId BS.NoPos $ fromString
                        $ specTypesPkgName prefix
      , BS.CImpId False $ BS.mkId BS.NoPos $ fromString
                        $ specIfcPkgName prefix
      , BS.CImpId False $ BS.mkId BS.NoPos "BluespecFP"
      ]

    moduleDef :: BS.CDefn
    moduleDef = BS.CValueSign $
      BS.CDef
        (BS.mkId BS.NoPos $ fromString $ "mk" ++ prefix)
        -- :: Module <prefix>Ifc -> Module Empty
        (BS.CQType
          []
          (BS.tArrow
            `BS.TAp` (BS.tModule `BS.TAp` ifcTy)
            `BS.TAp` (BS.tModule `BS.TAp` emptyTy)))
        [ BS.CClause [BS.CPVar ifcModId] [] $
          BS.Cmodule BS.NoPos $
              BS.CMStmt
                (BS.CSBind (BS.CPVar ifcArgId) Nothing [] (BS.CVar ifcModId))
            : map BS.CMStmt mkGlobals ++
            [ BS.CMStmt $ BS.CSletrec genFuns
            , BS.CMrules $ BS.Crules [] rules
            ]
        ]

    ifcArgId = BS.mkId BS.NoPos $ fromString ifcArgName
    ifcModId = BS.mkId BS.NoPos "ifcMod"

    rules :: [BS.CRule]
    rules = map mkTriggerRule uniqueTriggers ++ maybeToList (mkStepRule streams)

    streams        = specStreams spec
    triggers       = specTriggers spec
    uniqueTriggers = mkUniqueTriggers triggers
    exts           = gatherExts streams triggers

    ifcId     = BS.mkId BS.NoPos $ fromString $ specIfcName prefix
    ifcFields = mkSpecIfcFields triggers exts
    ifcTy     = BS.TCon (BS.TyCon
                  { BS.tcon_name = ifcId
                  , BS.tcon_kind = Just BS.KStar
                  , BS.tcon_sort = BS.TIstruct
                                     (BS.SInterface [])
                                     (map BS.cf_name ifcFields)
                  })

    emptyTy = BS.TCon (BS.TyCon
                { BS.tcon_name = BS.idEmpty
                , BS.tcon_kind = Just BS.KStar
                , BS.tcon_sort = BS.TIstruct (BS.SInterface []) []
                })

    -- Make buffer and index declarations for streams.
    mkGlobals :: [BS.CStmt]
    mkGlobals = concatMap buffDecln streams ++ map indexDecln streams
      where
        buffDecln  (Stream sId buff _ ty) = mkBuffDecln  sId ty buff
        indexDecln (Stream sId _    _ _ ) = mkIndexDecln sId

    -- Make generator functions, including trigger arguments.
    genFuns :: [BS.CDefl]
    genFuns =  map accessDecln streams
            ++ map streamGen streams
            ++ concatMap triggerGen uniqueTriggers
      where
        accessDecln :: Stream -> BS.CDefl
        accessDecln (Stream sId buff _ ty) = mkAccessDecln sId ty buff

        streamGen :: Stream -> BS.CDefl
        streamGen (Stream sId _ expr ty) = mkGenFun (generatorName sId) expr ty

        triggerGen :: UniqueTrigger -> [BS.CDefl]
        triggerGen (UniqueTrigger uniqueName (Trigger _name guard args)) =
            guardDef : argDefs
          where
            guardDef = mkGenFun (guardName uniqueName) guard Bool
            argDefs  = map argGen (zip (argNames uniqueName) args)

            argGen :: (String, UExpr) -> BS.CDefl
            argGen (argName, UExpr ty expr) = mkGenFun argName expr ty

-- | Generate a @<prefix>Ifc.bs@ file from a 'Spec'. This contains the
-- definition of the @<prefix>Ifc@ interface, which declares the types of all
-- trigger functions and external variables. This is put in a separate file so
-- that larger applications can use it separately.
compileIfcBS :: BluespecSettings -> String -> Spec -> BS.CPackage
compileIfcBS _bsSettings prefix spec =
    BS.CPackage
      ifcPkgId
      (Right [])
      (stdLibImports ++ genImports)
      []
      [ifcDef]
      []
  where
    -- import <prefix>Types
    genImports :: [BS.CImport]
    genImports =
      [ BS.CImpId False $ BS.mkId BS.NoPos $ fromString
                        $ specTypesPkgName prefix
      ]

    ifcId     = BS.mkId BS.NoPos $ fromString $ specIfcName prefix
    ifcPkgId  = BS.mkId BS.NoPos $ fromString $ specIfcPkgName prefix
    ifcFields = mkSpecIfcFields triggers exts

    streams  = specStreams spec
    exts     = gatherExts streams triggers

    -- Remove duplicates due to multiple guards for the same trigger.
    triggers = nubBy compareTrigger (specTriggers spec)

    ifcDef :: BS.CDefn
    ifcDef = BS.Cstruct
               True
               (BS.SInterface [])
               (BS.IdK ifcId)
               [] -- No type variables
               ifcFields
               [] -- No derived instances

-- | Generate a @<prefix>Types.bs@ file from a 'Spec'. This declares the types
-- of any structs used by the Copilot specification. This is put in a separate
-- file so that larger applications can more easily substitute their own struct
-- definitions if desired.
compileTypesBS :: BluespecSettings -> String -> Spec -> BS.CPackage
compileTypesBS _bsSettings prefix spec =
    BS.CPackage
      typesId
      (Right [])
      stdLibImports
      []
      structDefs
      []
  where
    typesId = BS.mkId BS.NoPos $ fromString $ specTypesPkgName prefix

    structDefs = mkTypeDeclns exprs

    exprs    = gatherExprs streams triggers
    streams  = specStreams spec

    -- Remove duplicates due to multiple guards for the same trigger.
    triggers = nubBy compareTrigger (specTriggers spec)

    -- Generate type declarations.
    mkTypeDeclns :: [UExpr] -> [BS.CDefn]
    mkTypeDeclns es = catMaybes $ map mkTypeDecln uTypes
      where
        uTypes = nub $ concatMap (\(UExpr _ e) -> exprTypes e) es

        mkTypeDecln (UType ty) = case ty of
          Struct x -> Just $ mkStructDecln x
          _        -> Nothing

-- | Imports from the Bluespec standard library.
stdLibImports :: [BS.CImport]
stdLibImports =
  [ BS.CImpId False $ BS.mkId BS.NoPos "FloatingPoint"
  , BS.CImpId False $ BS.mkId BS.NoPos "Vector"
  ]

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
