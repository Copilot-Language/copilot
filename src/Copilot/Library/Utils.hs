module Copilot.Library.Utils
    ( take, tails, nfoldl, nfoldl1, nfoldr, nfoldr1,
      scanl, scanr, scanl1, scanr1 ) where


import Copilot.Language
import Copilot.Language.Prelude hiding ( take )
import qualified Prelude as P


-- | functions similar to the Prelude functions on lists

tails :: ( Typed a )
         => Stream a -> [ Stream a ]
tails s = [ drop x s | x <- [ 0 .. ] ]


take :: ( Integral a, Typed b )
        => a -> Stream b -> [ Stream b ]
take n s = P.take ( fromIntegral n ) $ tails s


nfoldl :: ( Typed a )
          => Int -> ( Stream a -> Stream a -> Stream a )
                 ->   Stream a -> Stream a -> Stream a
nfoldl n f e s = foldl f e $ take n s


nfoldl1 :: ( Typed a )
           => Int -> ( Stream a -> Stream a -> Stream a )
                  ->   Stream a -> Stream a
nfoldl1 n f s = foldl1 f $ take n s


nfoldr :: ( Typed a )
          => Int -> ( Stream a -> Stream a -> Stream a )
                 ->   Stream a -> Stream a -> Stream a
nfoldr n f e s = foldr f e $ take n s


nfoldr1 :: ( Typed a )
           => Int -> ( Stream a -> Stream a -> Stream a )
                  ->   Stream a -> Stream a
nfoldr1 n f s = foldr1 f $ take n s


nscanl :: ( Typed a )
          => Int -> ( Stream a -> Stream a -> Stream a )
          -> Stream a -> Stream a -> [ Stream a ]
nscanl n f e s = scanl f e $ take n s


nscanr :: ( Typed a )
          => Int -> ( Stream a -> Stream a -> Stream a )
          -> Stream a -> Stream a -> [ Stream a ]
nscanr n f e s = scanr f e $ take n s


nscanl1 :: ( Typed a )
           => Int -> ( Stream a -> Stream a -> Stream a )
           -> Stream a -> [ Stream a ]
nscanl1 n f s = scanl1 f $ take n s


nscanr1 :: ( Typed a )
           => Int -> ( Stream a -> Stream a -> Stream a )
           -> Stream a -> [ Stream a ]
nscanr1 n f s = scanr1 f $ take n s
