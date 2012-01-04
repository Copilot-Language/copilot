-- | The examples are for testing the ptLTL library

module PTLTLExamples ( ptltlExamples ) where 

import Language.Copilot
import Prelude ()
import qualified Data.List as L


-- | test of previous
previousTestData :: Stream Bool
previousTestData = [ True, False ] ++ previousTestData

previousTest :: Spec
previousTest = do
  observer "previousTest" ( previous previousTestData )


-- | test of alwaysBeen
alwaysBeenTestData ::  Stream Bool
alwaysBeenTestData = [ True, True, True, True, True, True, True, False ]
                     ++ alwaysBeenTestData

alwaysBeenTest :: Spec
alwaysBeenTest = do
  observer "testAlwaysBeen" ( alwaysBeen alwaysBeenTestData )


-- | test of eventuallyPrevious
eventuallyPrevTestData ::  Stream Bool
eventuallyPrevTestData = [ False, False, False, False, False, True, False ]
                         ++ eventuallyPrevTestData

eventuallyPrevTest :: Spec
eventuallyPrevTest = observer "eventuallyPrevTest"
                     ( eventuallyPrev eventuallyPrevTestData )


-- | test of since
sinceTestData1 :: Stream Bool
sinceTestData1 = [ False, False, False ] ++ true

sinceTestData2 :: Stream Bool
sinceTestData2 = [ False, False, True, False, False, False, False ]
                 ++ sinceTestData2

sinceTest :: Spec
sinceTest = observer "sinceTest"
            ( sinceTestData1 `since` sinceTestData2 )


-- | test since with external variables
sinceExtTest :: Spec
sinceExtTest = observer "sinceExtTest"
               ( extern "e1" (Just e1) `since` extern "e2" (Just e2))

-- | external variables
e1, e2 :: [Bool]
e1 = replicate 10 False L.++               repeat True
e2 = replicate 9  False L.++ [ True ] L.++ repeat False

ptltlExamples :: IO ()
ptltlExamples = do
  prettyPrint previousTest
  interpret 20 previousTest

  prettyPrint alwaysBeenTest
  interpret 20 alwaysBeenTest

  prettyPrint eventuallyPrevTest
  interpret 20 eventuallyPrevTest

  prettyPrint sinceTest
  interpret 20 sinceTest

  prettyPrint sinceExtTest
  interpret 20  sinceExtTest

