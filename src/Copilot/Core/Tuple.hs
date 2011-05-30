-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | 

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Tuple
  ( Tup2 (..)
  , Tup3 (..)
  , Tup4 (..)
  ) where

data Tup2 α β = Tup2
  { untup2_1 :: !α
  , untup2_2 :: !β
  } deriving (Eq, Show)

data Tup3 α β γ = Tup3
  { untup3_1 :: !α
  , untup3_2 :: !β
  , untup3_3 :: !γ
  } deriving (Eq, Show)

data Tup4 α β γ δ = Tup4
  { untup4_1 :: !α
  , untup4_2 :: !β
  , untup4_3 :: !γ
  , untup4_4 :: !δ
  } deriving (Eq, Show)
