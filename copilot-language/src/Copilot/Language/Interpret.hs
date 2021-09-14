{- Copyright (c) 2011 National Institute of Aerospace / Galois, Inc. -}

-- | This module implements two interpreters, which may be used to simulated or
-- executed Copilot specifications on a computer to understand their behavior
-- to debug possible errors.
--
-- The interpreters included vary in how the present the results to the user.
-- One of them uses a format (csv) that may be more machine-readable, while the
-- other uses a format that may be easier for humans to read.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs, FlexibleInstances #-}

module Copilot.Language.Interpret
  ( csv
  , interpret
  ) where

import qualified Copilot.Core.Interpret as I

import Copilot.Language.Spec (Spec)
import Copilot.Language.Reify

--------------------------------------------------------------------------------

-- | Simulate a number of steps of a given specification, printing the results
-- in a table in comma-separated value (CSV) format.
csv :: Integer -> Spec -> IO ()
csv i spec = do
  putStrLn "Note: CSV format does not output observers."
  interpret' I.CSV i spec

--------------------------------------------------------------------------------

-- | Simulate a number of steps of a given specification, printing the results
-- in a table in readable format.
--
-- Compared to 'csv', this function is slower but the output may be more
-- readable.
interpret :: Integer -> Spec -> IO ()
interpret = interpret' I.Table

-- | Simulate a number of steps of a given specification, printing the results
-- in the format specified.
interpret' :: I.Format -> Integer -> Spec -> IO ()
interpret' format i spec = do
  coreSpec <- reify spec
  putStrLn $ I.interpret format (fromIntegral i) coreSpec

--------------------------------------------------------------------------------
