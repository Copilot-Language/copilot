{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleContexts #-}

module Copilot.Compile.C.Util where

import Copilot.Compile.C.Tmp
import Copilot.Compile.C.Translation

import Copilot.Core

import Language.C99 as C hiding (Struct)
import Language.C99.Util

{- TODO: move these functions -}
ptrdeclr :: String -> Maybe Init -> InitDeclr
ptrdeclr n = _declr n (Just (PtrBase Nothing))

declr :: String -> Maybe Init -> InitDeclr
declr n = _declr n Nothing

_declr :: String -> Maybe Ptr -> Maybe Init -> InitDeclr
_declr n ptr Nothing  = InitDeclr (Declr ptr (DirectDeclrIdent $ ident n))
_declr n ptr (Just i) = InitDeclrInitr  (Declr ptr (DirectDeclrIdent $ ident n)) i

arrdeclr :: String -> [Int] -> Maybe Init -> InitDeclr
arrdeclr name (l:ls) init = case init of
  Just init' -> InitDeclrInitr (Declr Nothing dds) init'
  Nothing    -> InitDeclr (Declr Nothing dds)
  where
    dds = foldl cons base ls
    base = DirectDeclrArray1 (DirectDeclrIdent $ ident name) Nothing (len l)
    cons xs l = DirectDeclrArray1 xs Nothing (len l)
    len :: Int -> Maybe AssignExpr
    len n = Just $ wrap $ constty Int32 (fromIntegral n)

vardef :: DeclnSpecs -> [InitDeclr] -> Decln
vardef ds []       = Decln ds Nothing
vardef ds (i:ids)  = Decln ds dl where
  dl = Just $ foldl InitDeclrCons (InitDeclrBase i) ids

{- Combine droplengths with the right stream -}
combine :: [(Id, Word32, String)] -> [Stream] -> [(Stream, Word32, String)]
combine xs ss = map (\(i,d,name) -> (findstream i ss, d,name)) xs


index :: String -> C.Expr -> C.Expr
index arr e = wrap $ PostfixIndex (wrap $ PrimIdent $ ident arr) (wrap e)

{- Take a type and a value, and return a literal data init -}
initval :: Type a -> a -> Init
initval ty x = case ty of
  Array _   -> InitArray $ initlist $ arraydata x
  Struct _  -> InitArray $ initlist $ structdata x
  otherwise -> InitExpr $ wrap $ constty ty x

{- Take a type and a list of values to construct init data of stream -}
initvals :: Type a -> [a] -> Init
initvals ty xs = InitArray $ initlist $ map (initval ty) xs

{- Create init data for struct -}
structdata :: Struct a => a -> [Init]
structdata xs = map f (toValues xs) where
  f (Value ty (Field v)) = case ty of
    Array _   -> InitArray $ initlist $ arraydata v
    Struct _  -> InitArray $ initlist $ structdata v
    otherwise -> InitExpr $ wrap $ constty ty v

{- Create init data for array -}
arraydata :: forall a n b. (
    Flatten a b
  , b ~ InnerType a
  , Typed a
  , Typed b
  ) => Array n a -> [Init]
arraydata xs = map f (flat xs) where
  flat :: Array n a -> [b]
  flat = flatten
  f :: Typed b => b -> Init
  f x = InitExpr $ wrap $ constty typeOf x

{- Create InitList from list of Inits -}
initlist :: [Init] -> InitList
initlist (i:is) = foldl cons base is where
  base = InitBase Nothing i
  cons xs x = InitCons xs Nothing x


basevar :: Id -> String
basevar i = "s" ++ show i

locvar :: String -> String
locvar base = base ++ "_loc"

idxvar :: String -> String
idxvar base = base ++ "_idx"

excpy :: String -> String
excpy name = name ++ "_cpy"

dropname id n = locvar $ basevar id ++ case n > 0 of
  True  -> "drop" ++ show n
  False -> []
