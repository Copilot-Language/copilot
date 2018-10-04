{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module Copilot.Compile.C.CodeGen
  ( gather
  , vars
  , funcs
  , typedeclns
  , reify
  , headerfile
  , mkfunargs
  , mkstructdecln
  ) where

import Copilot.Core as CP hiding (index, ExtFun, toList)

import Copilot.Compile.C.Tmp
import Copilot.Compile.C.Util
import Copilot.Compile.C.Translation
import Copilot.Compile.C.Meta

import Copilot.Compile.ACSL.CodeGen
import Text.PrettyPrint

import Language.C99 as C  ( BlockItem     (..)
                              , Decln
                              , ExtDecln
                              , Expr          (..)
                              , Init          (..)
                              , FunDef        (..)
                              , Declr         (..)
                              , DirectDeclr   (..)
                              , CompoundStmt  (..)
                              , Stmt          (..)
                              , Ptr           (..)
                              , JumpStmt  (JumpReturn)
                              , ExprStmt (..)
                              , SelectStmt  (SelectIf)
                              , InitDeclr (..)
                              , ExtDecln (..)
                              , Decln ( Decln )
                              , PrimExpr    (..)
                              , PostfixExpr (..)
                              , AddExpr     (..)
                              , MultExpr    (..)
                              , UnaryExpr   (..)
                              , TypeSpec    (..)
                              , TypedefName (..)
                              , StructOrUnionSpec (..)
                              , StructOrUnion (..)
                              )
import Language.C99.Util hiding (vardef, var, funcall, Var)

import Control.Monad.State ( State
                           , put
                           , get
                           , runState
                           )
import Data.List (union, unionBy, nub)
import Data.Typeable (Typeable)

import GHC.Exts


{- Attribute Grammar like state -}
data FunEnv = FunEnv
  { stmts   :: [BlockItem]
  , ids     :: [(Id, Word32, String)]
  }

type FunState = State FunEnv
emptyFunState :: FunEnv
emptyFunState = FunEnv { stmts = []
                       , ids   = []
                       }

{- Concrete program, used to list global variables and functions -}
data Program = Program
  { vars       :: [Decln]
  , funcs      :: [FunDef]
  , typedeclns :: [Decln]
  }


{- Translate Copilot expression to a C-expression -}
cexpr :: CP.Expr a -> FunState C.Expr
cexpr (Const ty x) = return $ constty ty x

cexpr (Local ty1 ty2 n e1 e2) = do
  let cty = ty2type ty1
  e1' <- cexpr e1
  env <- get
  let decln = BlockItemDecln $ vardef cty [declr n (Just $ InitExpr $ wrap e1')]
  put $ env { stmts = (stmts env) ++ [decln] }
  e2' <- cexpr e2
  return $ wrap e2'

cexpr (Var ty n)   = return $ var n

cexpr (Drop ty n id) = do
  locname <- fresh id n
  return $ var locname where
    {- Generate a fresh name, if it is already in use.
       To avoid conflicts as much as possible, we append "_dropN" if n > 0. -}
    fresh :: Id -> Word32 -> FunState String
    fresh id drop = do
      env <- get
      let usednames = map third (ids env)
          dropped = dropname id n
          dropped_fresh x = dropped ++ "_" ++ (show x)
          locname = head $ dropWhile (flip elem usednames) freshnames
          freshnames = dropped:(dropped_fresh <$> [0..])
      put $ env { ids = (ids env) `union` [(id, drop, locname)] }
      return locname
    third (a,b,c) = c

cexpr (ExternVar ty n args) = return $ var (excpy n)

cexpr (Op1 op e) = do
  e' <- cexpr e
  return $ op1 op e'

cexpr (Op2 op e1 e2) = do
  e1' <- cexpr e1
  e2' <- cexpr e2
  return $ op2 op e1' e2'

cexpr (Op3 op e1 e2 e3) = do
  e1' <- cexpr e1
  e2' <- cexpr e2
  e3' <- cexpr e3
  return $ op3 op e1' e2' e3'


{- Gather required information from Spec, and create names for functions -}
gather :: Spec -> AProgram
gather spec = AProgram  { streams     = streams
                        , generators  = map genname streams
                        , trigguards  = map guardname triggers
                        , externals   = externals'
                        , types       = types
                        } where
  streams = specStreams spec
  triggers = specTriggers spec

  genname :: Stream -> Generator
  genname s = Generator { genValName    = basename
                        , genBuffName   = basename ++ "_buff"
                        , genIndexName  = idxvar basename
                        , genFuncName   = basename ++ "_gen"
                        , genStream     = s
                        } where
    basename = basevar (streamId s)

  guardname :: Trigger -> Guard
  guardname t = Guard { guardName    = triggerName t ++ "_guard"
                      , guardCFunc   = triggerName t
                      , guardArgs    = argnames t
                      , guardTrigger = t
                      }

  argnames :: Trigger -> [Argument]
  argnames (Trigger name guard args) = args' where
    args' = map argname (zip [0..] args)
    argname (n, a) = Argument { argName = name ++ "_arg" ++ show n
                              , argExpr = a
                              }

  -- Gather all types that are used
  types :: [UType]
  types = nub $ concat $ map (gathertypes.triggerGuard) triggers
          ++ concatMap (\xs -> map f (triggerArgs xs)) triggers
          ++ map (\(Stream _ _ e _) -> gathertypes e) streams where
    f (UExpr _ e) = gathertypes e

  externals' :: [External]
  externals' = unions' $ (map exstreams streams) ++ (map extriggers triggers)
  exstreams (Stream _ _ e _) = externs e
  extriggers (Trigger _ guard args) = unions' (externs guard : map exuexprs args)
  exuexprs (UExpr _ e) = externs e

  externs :: CP.Expr a -> [External]
  externs e = case e of
    Local _ _ _ e1 e2   -> externs e1 `union'` externs e2
    ExternVar t name _  -> [ External { exName    = name
                                      , exLocName = excpy name
                                      , exType    = UType t
                                      } ]
    Op1 _ e             -> externs e
    Op2 _ e1 e2         -> externs e1 `union'` externs e2
    Op3 _ e1 e2 e3      -> externs e1 `union'` externs e2 `union'` externs e3
    _                   -> []

  union' = unionBy (\a b -> exName a == exName b)
  unions' = foldr union' []

{- Translate abstract program to a concrete one -}
reify :: AProgram -> Program
reify ap = Program  { funcs = concat $  [ map (streamgen ap) gens
                                        , map (guardgen ap) guards
                                        , map (arggen ap) args

                                        , [ step ap ]
                                        ]
                    , vars = globvars gens ++ extvars exts
                    , typedeclns = structdeclns types'
                    } where
  ss = streams ap
  gens   = generators ap
  guards = trigguards ap
  args   = concatMap guardArgs guards
  exts   = externals ap
  types' = types ap

  structdeclns :: [UType] -> [Decln]
  structdeclns tys = concatMap mkstruct tys where
    mkstruct (UType ty) = case ty of
      CP.Struct ty' -> [mkstructdecln ty]
      _             -> []

{- Global variables -}
globvars :: [Generator] -> [Decln]
globvars gens = buffs ++ vals ++ idxs where
  (buffs, vals, idxs) = unzip3 $ map globvar gens

  globvar :: Generator -> (Decln, Decln, Decln)
  globvar (Generator buffname val idx _ (Stream _ buff _ ty)) =
    let cty = ty2type ty
        len = length buff
    in case ty of
      Array _ -> let len' = [len, size $ head buff] in
        ( vardef (static $ cty)     [arrdeclr buffname len' (Just $ initvals ty buff)]
        , vardef (static $ cty)     [ptrdeclr val (Just $ InitExpr $ wrap $ index buffname (wrap $ constint 0))]
        , vardef (static $ size_t)  [declr idx (Just $ InitExpr $ wrap $ constint 0)]
        )

      _ ->
        ( vardef (static $ cty)     [arrdeclr buffname [len] (Just $ initvals ty buff)]
        , vardef (static $ cty)     [declr val (Just $ initval ty (head buff))]
        , vardef (static $ size_t)  [declr idx (Just $ InitExpr $ wrap $ constint 0)]
        )

{- Copies of external arrays -}
extvars :: [External] -> [Decln]
extvars exts = concatMap exarray exts where
  exarray :: External -> [Decln]
  exarray (External name locname (UType ty)) = let cty = ty2type ty in
    case ty of
      Array _ -> let len = tylength ty
                 in [ vardef (static $ cty) [arrdeclr locname [len] Nothing] ]
      _       -> [ vardef (static $ cty) [declr locname Nothing] ]

{- Create a function that generates the stream -}
streamgen :: AProgram -> Generator -> FunDef
streamgen ap g@(Generator _ _ _ func (Stream _ _ expr ty)) = fd where
  ss = streams ap
  fd = FunDef (static $ cty) dr Nothing body
  cty = ty2type ty
  dr = case ty of
    Array _ -> Declr (Just $ PtrBase Nothing) (DirectDeclrIdent $ ident func)
    _       -> Declr Nothing (DirectDeclrIdent $ ident func)
  body = fungen ss expr

{- Write function for the guard of a trigger -}
guardgen :: AProgram -> Guard -> FunDef
guardgen ap g@(Guard guardfunc _ _ (Trigger _ guard _)) = fd where
  ss = streams ap
  fd = fundef (static $ bool) guardfunc [] (toList body)
  (Compound (Just body)) = fungen ss guard

{- Write function that generates stream for argument of a trigger -}
arggen :: AProgram -> Argument -> FunDef
arggen ap a@(Argument funname (UExpr ty expr)) = fd where
  ss = streams ap
  fd = FunDef (static $ cty) dr Nothing body
  cty = ty2type ty
  dr = case ty of
    Array _ -> Declr (Just $ PtrBase Nothing) (DirectDeclrIdent $ ident funname)
    _       -> Declr Nothing (DirectDeclrIdent $ ident funname)
  body = fungen ss expr


type VarName = String
type Env = State [Id]

{- Generic function that writes the bodies of all generator / guard / args -}
fungen :: [Stream] -> CP.Expr a -> CompoundStmt
fungen ss expr = body where
  (e, env) = runState (cexpr expr) emptyFunState
  drops = combine (ids env) ss
  (bs, vars) = runState (mapM streambuff drops) []
  body = Compound $ Just $ fromList $ concat  [ map BlockItemDecln vars
                            , concat bs
                            , stmts env
                            , [ BlockItemStmt $ StmtJump $ JumpReturn $ Just e ]
                            ]

{- Code reading current value of a (dropped) stream -}
streambuff :: (Stream, Word32, String) -> State [Decln] [BlockItem]
streambuff (Stream i buff _ ty, drop, loc) = do
  let cty = ty2type ty
      basename = basevar i
      ptr = idxvar basename
      buffname = basename ++ "_buff"
      idx = "idx"
      dropped = "dropped"
      buffsize = length buff

      vars = case ty of
        Array _ -> [ vardef cty [ptrdeclr loc Nothing] ]
        _       -> [ vardef cty [declr    loc Nothing] ]

      body =  [ BlockItemStmt $ StmtCompound $ Compound $ Just $ fromList $ idxcode ++ [
                  BlockItemStmt $ assign (wrap $ PrimIdent $ ident loc) (wrap $ PostfixIndex (wrap $ PrimIdent $ ident buffname) (wrap $ var idx))
                ]
              ]

      idxcode = case drop of
        0 -> [ BlockItemDecln $ vardef size_t [declr idx (Just $ InitExpr $ wrap $ var ptr)] ]
        _ -> [ BlockItemDecln $ vardef size_t [declr dropped (Just $ InitExpr $ wrap $ AddPlus (wrap $ var ptr) (wrap $ constword drop))]
             , BlockItemDecln $ vardef size_t [declr idx (Just $ InitExpr $ wrap $ MultMod (wrap $ var dropped) (wrap $ constword buffsize))]
             ]

  vars' <- get
  put (vars ++ vars')
  return body



{- The step function updates the current state -}
step :: AProgram -> FunDef
step ap = fundef void "step" [] body where
  body = concat [ copyexts      exts
                , triggers      guards
                , update        gens
                , updatearrays  gens
                , updatebuffers gens
                , updateindices gens
                ] where
  gens = generators ap
  guards = trigguards ap
  exts = externals ap

  {- Copy external arrays to make monitor reentrant -}
  copyexts :: [External] -> [BlockItem]
  copyexts exts = concatMap copyext exts where
    copyext :: External -> [BlockItem]
    copyext (External name locname (UType ty)) = case ty of
      Array _ -> let l = tylength ty
                 in [ BlockItemStmt $ StmtExpr $ ExprStmt $ Just $ wrap $ funcall "memcpy" [ wrap $ var locname
                                                                    , wrap $ var name
                                                                    , wrap $ UnarySizeExpr (wrap $ var locname)
                                                                    ]
                    ]
      _       -> [ BlockItemStmt $ assign (wrap $ PrimIdent $ ident locname) (wrap $ PrimIdent $ ident name) ]

  {- Check guards and fire triggers accordingly -}
  triggers :: [Guard] -> [BlockItem]
  triggers gs = map triggercond gs where
    triggercond (Guard guardname cfunc args _) = BlockItemStmt $ StmtSelect $ SelectIf (wrap cond) call where
      cond = funcall guardname []
      call = StmtExpr $ ExprStmt $ Just $ wrap $ funcall cfunc args'
      args' = map (\a -> wrap $ funcall (argName a) []) args

  {- Update stream values -}
  update :: [Generator] -> [BlockItem]
  update gens = map update' gens where
    update' gen = BlockItemStmt $ assign (wrap $ PrimIdent $ ident $ genValName gen) (wrap $ funcall (genFuncName gen) [])

  {- Copy current value of array to tmp value -}
  updatearrays :: [Generator] -> [BlockItem]
  updatearrays gens = concatMap updatearray gens where
    updatearray :: Generator -> [BlockItem]
    updatearray (Generator buff val idx _ (Stream _ b _ ty)) = case ty of
      Array tya ->
        [ BlockItemDecln $ vardef (ty2type tya) [
            InitDeclr $ Declr Nothing (DirectDeclrArray1 (DirectDeclrIdent $ ident tmp) Nothing (Just $ wrap $ constint $ fromIntegral l))
          ]
        , BlockItemStmt $ StmtExpr $ ExprStmt $ Just $ wrap $ funcall "memcpy" [wrap $ var tmp, wrap $ var val, wrap $ UnarySizeExpr (wrap $ var tmp)]
        ] where
          size = UnarySizeExpr (wrap $ index buff (wrap $ constint 0))
          tmp = val ++ "_tmp"
          idxbuff = index buff (var idx)
          l = tylength ty
      _ -> []

  {- Update buffers -}
  updatebuffers :: [Generator] -> [BlockItem]
  updatebuffers gens = map updatebuffer gens where
    updatebuffer (Generator buff val idx _ (Stream _ b _ ty)) = stmt where
      idxbuff = index buff (var idx)
      tmp = val ++ "_tmp"
      size = UnarySizeExpr (wrap $ var tmp)
      stmt = case ty of
        Array _ -> BlockItemStmt $ StmtExpr $ ExprStmt $ Just $ wrap $ funcall "memcpy" [wrap idxbuff, wrap $ var tmp, wrap $ size]
        _       -> BlockItemStmt $ assign idxbuff (wrap $ var val)

  {- Update indices / pointers in the arrays -}
  updateindices :: [Generator] -> [BlockItem]
  updateindices gens = incs ++ mods where
    (incs, mods) = unzip $ map updateindex gens
    updateindex (Generator _ _ idx _ (Stream _ buff _ _)) =
      ( BlockItemStmt $ StmtExpr $ ExprStmt $ Just $ wrap (UnaryInc $ wrap $ var idx)
      , BlockItemStmt $ assign (wrap $ PrimIdent $ ident idx) (wrap $ MultMod (wrap $ var idx) (wrap $ constint (fromIntegral buffsize)))
      ) where
        buffsize = length buff


{- Generates header file: (variables, triggers, step-function) -}
headerfile :: AProgram -> ([ExtDecln], [ExtDecln], ExtDecln)
headerfile ap = ( map vardecln     (externals ap)
                , map triggerdecln triggers
                , ExtFun $ fundeclr void "step" []
                ) where
    vardecln (External name _ (UType ty)) = ExtDecln $ vardef ty' [varname] where
      varname = declr name Nothing
      ty' = extern $ ty2type ty

    triggerdecln (Trigger name _ args) = ExtFun $ fundeclr void name args' where
      args' = mkfunargs (zip args argnames)
      argnames = map (\n -> "arg" ++ (show n)) [0..]

    triggers = map guardTrigger (trigguards ap)


{- Create a list of typed and named function arguments -}
mkfunargs :: [(UExpr, String)] -> [Decln]
mkfunargs args = map mkarg args where
  mkarg :: (UExpr, String) -> Decln
  mkarg (UExpr ty _, name) = vardef (ty2type ty) d where
    d = case ty of
      Array _ -> [ptrdeclr name Nothing]
      _       -> [declr    name Nothing]


{- Gather all used types in an expression -}
gathertypes :: Typeable a => CP.Expr a -> [UType]
gathertypes e = case e of
  Const ty _            -> [UType ty]
  Local ty1 ty2 _ e1 e2 -> [UType ty1, UType ty2] ++ gathertypes e1 ++ gathertypes e2
  Var ty _              -> [UType ty]
  Drop ty _ _           -> [UType ty]
  ExternVar ty _ _      -> [UType ty]
  Op1 _ e1              -> gathertypes e1
  Op2 _ e1 e2           -> gathertypes e1 ++ gathertypes e2
  Op3 _ e1 e2 e3        -> gathertypes e1 ++ gathertypes e2 ++ gathertypes e3

mkstructdecln :: Struct a => Type a -> Decln
mkstructdecln (CP.Struct ty) = structdecln (typename ty) (map f fields) where
  fields = toValues ty
  f (Value fty v) = case fty of
    Array _ -> fielddef (ty2typespec fty) (Just $ PtrBase Nothing) (fieldname v)
    _       -> fielddef (ty2typespec fty) Nothing                  (fieldname v)

  -- TODO: (re)move
  ty2typespec :: Type a -> TypeSpec
  ty2typespec ty = let td name = TTypedef (TypedefName $ ident name) in case ty of
    Int8      -> td "int8_t"
    Int16     -> td "int16_t"
    Int32     -> td "int32_t"
    Int64     -> td "int64_t"
    Word8     -> td "uint8_t"
    Word16    -> td "uint16_t"
    Word32    -> td "uint32_t"
    Word64    -> td "uint64_t"
    Float     -> TFloat
    Double    -> TDouble
    Bool      -> td "bool"
    Array tya -> ty2typespec tya
    CP.Struct s  -> TStructOrUnion $ StructOrUnionForwDecln C.Struct (ident $ typename s)
