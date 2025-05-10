-- | Bluespec backend specific versions of selected `Copilot.Core` datatypes.
module Copilot.Compile.Bluespec.Representation
    ( UniqueTrigger (..)
    , UniqueTriggerId
    , mkUniqueTriggers
    )
  where

import Copilot.Core ( Trigger (..) )

-- | Internal unique name for a trigger.
type UniqueTriggerId = String

-- | A `Copilot.Core.Trigger` with an unique name.
data UniqueTrigger = UniqueTrigger UniqueTriggerId Trigger

-- | Given a list of triggers, make their names unique.
mkUniqueTriggers :: [Trigger] -> [UniqueTrigger]
mkUniqueTriggers ts = zipWith mkUnique ts [0..]
  where
    mkUnique :: Trigger -> Integer -> UniqueTrigger
    mkUnique t@(Trigger name _ _) n = UniqueTrigger (name ++ "_" ++ show n) t
