{-# LANGUAGE Rank2Types, FlexibleContexts, UndecidableInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Only exports instances for Show which allows the printing of streams
module Language.Copilot.PrettyPrinter () where

import Data.Int
import Data.Word
import Data.Map as M

import Language.Copilot.Core

instance (Show (a Bool), Show (a Int8), Show (a Int16), Show (a Int32), Show (a Int64),
    Show (a Word8), Show (a Word16), Show (a Word32), Show (a Word64),
    Show (a Float), Show (a Double)) => Show (StreamableMaps a) where
        show (SM bm i8m i16m i32m i64m w8m w16m w32m w64m fm dm) =
            let acc0 = M.foldrWithKey showVal "" bm
                acc1 = M.foldrWithKey showVal acc0 i8m
                acc2 = M.foldrWithKey showVal acc1 i16m
                acc3 = M.foldrWithKey showVal acc2 i32m
                acc4 = M.foldrWithKey showVal acc3 i64m
                acc5 = M.foldrWithKey showVal acc4 w8m
                acc6 = M.foldrWithKey showVal acc5 w16m
                acc7 = M.foldrWithKey showVal acc6 w32m
                acc8 = M.foldrWithKey showVal acc7 w64m
                acc9 = M.foldrWithKey showVal acc8 fm
                acc10 = M.foldrWithKey showVal acc9 dm
            in acc10
            where
                showVal :: (Streamable a, Show (b a)) => Var -> b a -> String -> String
                showVal v val string = v ++ " .= " ++ show val ++ "\n" ++ string

instance Show Streams where
  show s = show (getSpecs s)



