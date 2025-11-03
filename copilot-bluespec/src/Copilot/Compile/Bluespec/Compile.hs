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
           bsFile      = render $ pPrint $ compileBS      bsSettings prefix spec

       let dir = bluespecSettingsOutputDirectory bsSettings
       createDirectoryIfMissing True dir
       writeFile (dir </> specTypesPkgName prefix ++ ".bs") typesBsFile
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
-- the Bluespec backend. See the @copilot-bluespec/DESIGN.md@ document for a
-- high-level description of what this file contains.
compileBS :: BluespecSettings -> String -> Spec -> BS.CPackage
compileBS _bsSettings prefix spec =
    BS.CPackage
      (BS.mkId BS.NoPos (fromString prefix))
      (Right [])
      (stdLibImports ++ genImports)
      []
      [ ifcDef
      , mkModuleDefPragma
      , mkModuleDef
      , ifcRulesDef
      , mkModuleRulesDef
      , addModuleRulesDef
      ]
      []
  where
    -- import <prefix>Types
    genImports :: [BS.CImport]
    genImports =
      [ BS.CImpId False $ BS.mkId BS.NoPos $ fromString
                        $ specTypesPkgName prefix
      , BS.CImpId False $ BS.mkId BS.NoPos "BluespecFP"
      ]

    -- interface <prefix>Ifc {-# always_ready, always_enabled #-} =
    --   ...
    ifcDef :: BS.CDefn
    ifcDef = BS.Cstruct
               True
               (BS.SInterface [BS.PIAlwaysRdy, BS.PIAlwaysEnabled])
               (BS.IdK ifcId)
               [] -- No type variables
               ifcFields
               [] -- No derived instances

    -- {-# properties mkFibs = { verilog } #-}
    mkModuleDefPragma :: BS.CDefn
    mkModuleDefPragma = BS.CPragma $ BS.Pproperties mkModuleDefId [BS.PPverilog]

    -- mk<prefix> :: Module <prefix>Ifc
    -- mk<prefix> =
    --   module
    --     ...
    mkModuleDef :: BS.CDefn
    mkModuleDef = BS.CValueSign $
      BS.CDef
        mkModuleDefId
        (BS.CQType [] (BS.tModule `BS.TAp` ifcTy))
        [ BS.CClause [] [] $
            BS.Cmodule BS.NoPos $
              wireGlobalStmts ++ genFunStmts ++ ruleIfcStmts
        ]
      where
        wireGlobalStmts :: [BS.CMStmt]
        wireGlobalStmts = map BS.CMStmt (mkExtWires ++ mkGlobals)

        genFunStmts :: [BS.CMStmt]
        genFunStmts =
          -- language-bluespec's pretty-printer will error if it encounters a
          -- CSletrec with an empty list of definitions, so avoid generating a
          -- CSletrec if there are no streams.
          [ BS.CMStmt $ BS.CSletrec genFuns | not (null genFuns) ]

        ruleIfcStmts :: [BS.CMStmt]
        ruleIfcStmts =
          [ BS.CMrules $ BS.Crules [] $ maybeToList $ mkStepRule streams
          , BS.CMinterface $ BS.Cinterface BS.NoPos (Just ifcId) ifcMethodImpls
          ]

    -- interface <prefix>RulesIfc =
    --   ...
    ifcRulesDef :: BS.CDefn
    ifcRulesDef =
      BS.Cstruct
        True
        (BS.SInterface [])
        (BS.IdK ifcRulesId)
        [] -- No type variables
        ifcRulesFields
        [] -- No derived instances

    -- mk<prefix>Rules :: <prefix>Ifc -> <prefix>RulesIfc -> Rules
    -- mk<prefix>Rules ifc ifcRules =
    --   rules
    --     ...
    mkModuleRulesDef :: BS.CDefn
    mkModuleRulesDef =
      BS.CValueSign $
        BS.CDef
          mkModuleRulesDefId
          (BS.CQType [] mkModuleRulesType)
          [ BS.CClause
              (map BS.CPVar [ifcArgId, ifcRulesArgId])
              []
              (BS.Crules [] moduleRules)
          ]
      where
        -- <prefix>Ifc -> <prefix>RulesIfc -> Rules
        mkModuleRulesType :: BS.CType
        mkModuleRulesType =
          BS.tArrow `BS.TAp` ifcTy `BS.TAp`
            (BS.tArrow `BS.TAp` ifcRulesTy `BS.TAp` BS.tRules)

        -- rules
        --   ...
        moduleRules :: [BS.CRule]
        moduleRules = map mkTriggerRule uniqueTriggers ++ map mkExtRule exts

    -- add<prefix>Rules :: <prefix>Ifc -> <prefix>RulesIfc -> Module Empty
    -- add<prefix>Rules ifc ifcRules = addRules (mk<prefix>Rules ifc ifcRules)
    addModuleRulesDef :: BS.CDefn
    addModuleRulesDef =
      BS.CValueSign $
        BS.CDef
          addModuleRulesDefId
          (BS.CQType [] addModuleRulesType)
          [ BS.CClause
              (map BS.CPVar [ifcArgId, ifcRulesArgId])
              []
              addModuleRulesExpr
          ]
      where
        -- <prefix>Ifc -> <prefix>RulesIfc -> Module Empty
        addModuleRulesType :: BS.CType
        addModuleRulesType =
          BS.tArrow `BS.TAp` ifcTy `BS.TAp`
           (BS.tArrow `BS.TAp` ifcRulesTy `BS.TAp`
             (BS.tModule `BS.TAp` emptyTy))

        -- addRules (mk<prefix>Rules ifc ifcRules)
        addModuleRulesExpr :: BS.CExpr
        addModuleRulesExpr =
          BS.CApply
           (BS.CVar (BS.idAddRules BS.NoPos))
           [BS.CApply
             (BS.CVar mkModuleRulesDefId)
             (map BS.CVar [ifcArgId, ifcRulesArgId])]

    mkModuleDefId =
      BS.mkId BS.NoPos $ fromString $ "mk" ++ prefix
    mkModuleRulesDefId =
      BS.mkId BS.NoPos $ fromString $ "mk" ++ prefix ++ "Rules"
    addModuleRulesDefId =
      BS.mkId BS.NoPos $ fromString $ "add" ++ prefix ++ "Rules"

    streams        = specStreams spec
    triggers       = specTriggers spec
    uniqueTriggers = mkUniqueTriggers triggers
    exts           = gatherExts streams triggers

    -- Remove duplicates due to multiple guards for the same trigger.
    triggersNoDups = nubBy compareTrigger triggers

    ifcArgId      = BS.mkId BS.NoPos $ fromString ifcArgName
    ifcRulesArgId = BS.mkId BS.NoPos $ fromString ifcRulesArgName

    ifcId     = BS.mkId BS.NoPos $ fromString $ specIfcName prefix
    ifcFields = mkSpecIfcFields uniqueTriggers exts
    ifcTy     = BS.TCon (BS.TyCon
                  { BS.tcon_name = ifcId
                  , BS.tcon_kind = Just BS.KStar
                  , BS.tcon_sort = BS.TIstruct
                                     (BS.SInterface [])
                                     (map BS.cf_name ifcFields)
                  })

    ifcRulesId     = BS.mkId BS.NoPos $ fromString $ specIfcRulesName prefix
    ifcRulesFields = mkSpecIfcRulesFields triggersNoDups exts

    ifcRulesTy =
      BS.TCon $
        BS.TyCon
          { BS.tcon_name = ifcRulesId
          , BS.tcon_kind = Just BS.KStar
          , BS.tcon_sort =
              BS.TIstruct (BS.SInterface []) (map BS.cf_name ifcRulesFields)
          }

    emptyTy = BS.TCon (BS.TyCon
                { BS.tcon_name = BS.idEmpty
                , BS.tcon_kind = Just BS.KStar
                , BS.tcon_sort = BS.TIstruct (BS.SInterface []) []
                })

    -- Bind @Wire@ variables for each extern stream.
    mkExtWires :: [BS.CStmt]
    mkExtWires = map extWireStmt exts
      where
        extWireStmt :: External -> BS.CStmt
        extWireStmt (External name ty) = mkExtWireDecln name ty

    -- Make buffer and index declarations for streams.
    mkGlobals :: [BS.CStmt]
    mkGlobals = concatMap buffDecln streams ++ map indexDecln streams
      where
        buffDecln  (Stream sId buff _ ty) = mkBuffDecln  sId ty buff
        indexDecln (Stream sId _    _ _ ) = mkIndexDecln sId

    -- Make generator functions for streams.
    genFuns :: [BS.CDefl]
    genFuns = map accessDecln streams ++ map streamGen streams
      where
        accessDecln :: Stream -> BS.CDefl
        accessDecln (Stream sId buff _ ty) = mkAccessDecln sId ty buff

        streamGen :: Stream -> BS.CDefl
        streamGen (Stream sId _ expr ty) = mkGenFun (generatorName sId) expr ty

    -- Make interface methods for @<prefix>Ifc@.
    ifcMethodImpls :: [BS.CDefl]
    ifcMethodImpls =
        concatMap triggerMethodImpls uniqueTriggers
          ++ map extMethodImpl exts
      where
        -- interface
        --   ext val = ext_wire := val
        extMethodImpl :: External -> BS.CDefl
        extMethodImpl (External name _) =
            BS.CLValue extMethodId [extMethodClause] []
          where
            extMethodId = BS.mkId BS.NoPos (fromString name)
            valId       = BS.mkId BS.NoPos "val"

            -- ext val = ext_wire := val
            extMethodClause :: BS.CClause
            extMethodClause =
              BS.CClause
               [BS.CPVar valId]
               []
               (BS.Cwrite
                 BS.NoPos
                 (BS.CVar (BS.mkId BS.NoPos (fromString (wireName name))))
                 (BS.CVar valId))

        -- interface
        --   trig_guard = ...
        --   trig_arg0 = ...
        --   ...
        --   trig_arg(n-1) = ...
        triggerMethodImpls :: UniqueTrigger -> [BS.CDefl]
        triggerMethodImpls uniqueTrigger = guardDef : argDefs
          where
            UniqueTrigger uniqueName (Trigger _name guard args) = uniqueTrigger

            guardDef = mkGenFun (guardName uniqueName) guard Bool
            argDefs  = map argGen (zip (argNames uniqueName) args)

            argGen :: (String, UExpr) -> BS.CDefl
            argGen (argName, UExpr ty expr) = mkGenFun argName expr ty

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
