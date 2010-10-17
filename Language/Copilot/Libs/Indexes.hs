-- | Queries into how long until properties hold or fail.  We use Int16 to
-- return the value, so your queries must not require looking more than 32,767
-- periods :) .  Thus, in the following, the parameter @n@ must be @0 <= n <=
-- 32,767@.  -1 indicates the test failed.

module Language.Copilot.Libs.Indexes(soonest, soonestFail, latest, latestFail) where

import Prelude (id, Int, String, fromIntegral, ($))
import qualified Prelude as P 
import Data.Int (Int16)

import Language.Copilot.Libs.ErrorChks
import Language.Copilot.Core
import Language.Copilot.Language

soonestHlp :: String -> (Spec Bool -> Spec Bool) -> Int -> Spec Bool -> Spec Int16
soonestHlp name f n s = int16Chk name n $ nPosChk name n (buildStr 0)
  where buildStr m = 
          if m P.> n then const (-1)
             else mux (f $ drop m s) (const $ fI m) (buildStr (m P.+ 1))
        fI = fromIntegral

latestHlp :: String -> (Spec Bool -> Spec Bool) -> Int -> Spec Bool -> Spec Int16
latestHlp name f n s = int16Chk name n $ nPosChk name n (buildStr n)
  where buildStr m = 
          if m P.< 0 then const (-1)
             else mux (f $ drop m s) (const $ fI m) (buildStr (m P.- 1))
        fI = fromIntegral

-- | Returns the smallest @m <= n@ such that @drop m s@ is true, and @-1@ if no
-- such @m@ exists. 
soonest :: Int -> Spec Bool -> Spec Int16
soonest = soonestHlp "soonest" id

-- | Returns the smallest @m <= n@ such that @drop m s@ is false, and @-1@ if no
-- such @m@ exists.  
soonestFail :: Int -> Spec Bool -> Spec Int16
soonestFail = soonestHlp "soonestFail" not

-- | Returns the largest @m <= n@ such that @drop m s@ is true, and @-1@ if no
-- such @m@ exists.  
latest :: Int -> Spec Bool -> Spec Int16
latest = latestHlp "latest" id

-- | Returns the largest @m <= n@ such that @drop m s@ is false, and @-1@ if no
-- such @m@ exists.  
latestFail :: Int -> Spec Bool -> Spec Int16
latestFail = latestHlp "latest" not


                     

