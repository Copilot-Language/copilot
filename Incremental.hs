
-- Approach at RV with LTL by Bauer at al.
main :: IO ()
main = do
  interpret 50 spec
  spec' <- reify spec
  compile "c99" spec'

spec :: Spec
spec = do
  observer "formula"  (monitor [] (ltlGlobally LTLTrue))
  observer "counter"  count
  observer "max"      currentMax
  observer "oddities" oddities
  trigger "out" true [arg $ monitor [] (LTLTrue `LTLUntil` LTLTrue)]


-- Interesting example of the power of Copilot
count = mux (previousCounter == (1 :: Stream Word8))
            (previousMax + 1)
            (previousCounter - 1)

currentMax = mux (previousCounter == 1)
                 (previousMax + 1)
                 previousMax

previousCounter = [1] ++ count

previousMax = [0] ++ currentMax

oddities = currentMax `mod` 2 /= 0
