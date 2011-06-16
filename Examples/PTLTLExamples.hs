-- | The examples are for testing the ptLTL library

module Copilot.Examples.PTLTLExamples where


import Copilot.Language
import Copilot.Language.Prelude
import Prelude ( ($), (+) )
import Copilot.Library.PTLTL
import qualified Prelude   as P
import qualified Data.Bool as B
import qualified Data.List as L


-- | test of previous
previousTestData = [ True, False ] ++ previousTestData

previousTest :: Spec
previousTest = do
  trigger "previousTest" true [ arg $ previous previousTestData ]


-- | test of alwaysBeen
alwaysBeenTestData ::  Stream Bool
alwaysBeenTestData = [ True, True, True, True, True, True, True, False ]
                     ++ alwaysBeenTestData

alwaysBeenTest :: Spec
alwaysBeenTest = do
  trigger "testAlwaysBeen" true [ arg $ alwaysBeen alwaysBeenTestData ]


-- | test of eventuallyPrevious
eventuallyPrevTestData ::  Stream Bool
eventuallyPrevTestData = [ False, False, False, False, False, True, False ]
                         ++ eventuallyPrevTestData

eventuallyPrevTest :: Spec
eventuallyPrevTest = trigger "eventuallyPrevTest" true
                     [ arg $ eventuallyPrev eventuallyPrevTestData ]


-- | test of since
sinceTestData1 :: Stream Bool
sinceTestData1 = [ False, False, False ] ++ true

sinceTestData2 :: Stream Bool
sinceTestData2 = [ False, False, True, False, False, False, False ]
                 ++ sinceTestData2

sinceTest :: Spec
sinceTest = trigger "sinceTest" true
            [ arg $ sinceTestData1 `since` sinceTestData2 ]


-- | test since with external variables
sinceExtTest :: Spec
sinceExtTest = trigger "sinceExtTest" true
               [ arg $ extern "e1" `since` extern "e2" ]


-- "If the majority of the engine temperature probes exeeds 250 degrees, then
-- the cooler is engaged and remains engaged until the majority of the engine
-- temperature drops to 250 or below.  Otherwise, trigger an immediate shutdown
-- of the engine."

-- t0, t1, t2 :: Spec Word8
-- cooler, check, overHeat, monitor, maj :: Spec Bool
-- t0       = extW8 "temp_probe_0"
-- t1       = extW8 "temp_probe_1"
-- t2       = extW8 "temp_probe_2"
-- cooler   = extB  "fan_status"
-- -- Copilot vars
-- maj      = varB "maj"
-- check    = varB  "maj_check"
-- overHeat = varB  "over_heat"
-- monitor  = varB  "monitor"

-- engine :: Streams
-- engine = do
--   let temps = map (< 250) [t0, t1, t2]
--   maj      .= majority temps
--   check    .= aMajority temps maj
--   overHeat `ptltl` ((cooler || maj && check) `since` not maj)
--   monitor .= not overHeat
--   trigger monitor "shutoff" void

-- engineRun :: Bool -> IO ()
-- engineRun b =
--   if b
--      then interpret engine 20 $
--             setEB "fan_status" [False, False ..] $
--             setEW8 "temp_probe_0" [0 ..] $
--             setEW8 "temp_probe_1" [0 ..] $
--             setEW8 "temp_probe_2" [0 ..] baseOpts
--      else compile engine "engine" $ setSim 20 baseOpts


-- | external variables
e1 = replicate 10 False L.++               repeat True
e2 = replicate 9  False L.++ [ True ] L.++ repeat False

t0, t1, t2 :: [ Word8 ]
t0 = [ 0 .. ]
t1 = [ 0 .. ]
t2 = [ 0 .. ]

m :: [ Word8 ] -> [ Bool ]
m = map ( P.> 250 )

cooler = zipWith3
         ( \ x y z -> x B.|| y B.|| z )
         ( zipWith ( B.&& ) ( m t0 ) ( m t1 ) )
         ( zipWith ( B.&& ) ( m t1 ) ( m t2 ) )
         ( zipWith ( B.&& ) ( m t2 ) ( m t0 ) )

test = do
  prettyPrint previousTest
  interpret 20 [] previousTest

  prettyPrint alwaysBeenTest
  interpret 20 [] alwaysBeenTest

  prettyPrint eventuallyPrevTest
  interpret 20 [] eventuallyPrevTest

  prettyPrint sinceTest
  interpret 20 [] sinceTest

  prettyPrint sinceExtTest
  interpret 20 [ input "e1" e1, input "e2" e2 ] sinceExtTest

