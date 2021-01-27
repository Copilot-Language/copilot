--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Main import module for the front-end language.
--
-- This is a facility so that users of Copilot only need to @import Copilot@ in
-- their modules, together with the specific backend they intend to use.

{-# LANGUAGE Safe #-}

module Copilot
  ( module Copilot.Language
  ) where

import Copilot.Language

--------------------------------------------------------------------------------
