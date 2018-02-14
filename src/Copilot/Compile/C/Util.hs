{-# LANGUAGE GADTs #-}

module Copilot.Compile.C.Util where

import Copilot.Compile.C.Tmp
import Copilot.Compile.C.Translation

import Copilot.Core

import Language.C99.AST as C hiding (Struct)
import Language.C99.Util

{- TODO: move these functions -}
ptrdeclr :: String -> Maybe Init -> InitDeclr
ptrdeclr n = _declr n (Just (PBase Nothing))

declr :: String -> Maybe Init -> InitDeclr
declr n = _declr n Nothing

_declr :: String -> Maybe Ptr -> Maybe Init -> InitDeclr
_declr n ptr Nothing  = IDDeclr (Dr ptr (DDIdent $ ident n))
_declr n ptr (Just i) = IDInit  (Dr ptr (DDIdent $ ident n)) i

arrdeclr :: String -> [Int] -> Init -> InitDeclr
arrdeclr name (l:ls) init = IDInit (Dr Nothing dds) init where
  dds = foldl cons base ls
  base = DDArray1 (DDIdent $ ident name) Nothing (len l)
  cons xs l = DDArray1 xs Nothing (len l)
  len n = Just $ constty Int32 (fromIntegral n)

vardef :: DeclnSpecs -> [InitDeclr] -> Decln
vardef ds []       = Dn ds Nothing
vardef ds (i:ids)  = Dn ds dl where
  dl = Just $ foldl IDLCons (IDLBase i) ids

{- Combine droplengths with the right stream -}
combine :: [(Id, Word32)] -> [Stream] -> [(Stream, Word32)]
combine xs ss = map (\(i,d) -> (findstream i ss, d)) xs


index :: String -> C.Expr -> C.Expr
index arr e = EIndex (EIdent $ ident arr) e

{- Take a type and a value, and return a literal data init -}
initval :: Type a -> a -> Init
initval ty x = case ty of
  Array _   -> IArray $ initlist $ arraydata x
  Struct _  -> IArray $ initlist $ structdata x
  otherwise -> IExpr $ constty ty x

{- Take a type and a list of values to construct init data of stream -}
initvals :: Type a -> [a] -> Init
initvals ty xs = IArray $ initlist $ map (initval ty) xs

{- Create init data for struct -}
structdata :: Struct a => a -> [Init]
structdata xs = map f (toValues xs) where
  f (V ty n v) = case ty of
    Array _   -> IArray $ initlist $ arraydata v
    otherwise -> IExpr $ constty ty v

{- Create init data for array -}
arraydata :: Typed a => Array i a -> [Init]
arraydata xs = map f (toList xs) where
  f x = IExpr $ constty typeOf x

{- Create InitList from list of Inits -}
initlist :: [Init] -> InitList
initlist (i:is) = foldl cons base is where
  base = InitLBase Nothing i
  cons xs x = InitLCons xs Nothing x

