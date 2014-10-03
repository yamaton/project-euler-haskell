{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens               ((^.))
import qualified Control.Monad              as Monad
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.Char                  as Char
import qualified Data.IntSet                as IntSet
import qualified Data.List                  as List
import qualified Data.Graph                 as Graph
import qualified Data.Number.CReal          as CReal
import qualified Data.Ord                   as Ord
import qualified Data.Ratio                 as Ratio
import qualified Data.Set                   as Set
import qualified Network.Wreq               as Wreq
import           System.Environment         (getArgs)
import qualified Utils

-- | Problem 71
-- [Ordered fractions](http://projecteuler.net/problem=71)
prob071 :: Int
prob071 = undefined




-- | Problem 72
-- [Counting fractions](http://projecteuler.net/problem=72)
prob072 :: Int
prob072 = undefined


-- | Problem 73
-- [Counting fractions in a range](http://projecteuler.net/problem=73)
prob073 :: Int
prob073 = Set.size . Set.fromList $ concatMap bound [1..12000]
  where
    delta = 1.0e-6
    lower = 1/3 + delta
    upper = 1/2 - delta
    bound :: Int -> [Ratio.Ratio Int]
    bound i = map (Ratio.% i) [ceiling (fromIntegral i * lower) .. floor (fromIntegral i * upper)]


-- | Problem 74
-- [Digit factorial chains](http://projecteuler.net/problem=74)
prob074 :: Int
prob074 = undefined


-- | Problem 75
-- [Singular integer right triangles](http://projecteuler.net/problem=75)
prob075 :: Int
prob075 = undefined


-- | Problem 76
-- [Counting summations](http://projecteuler.net/problem=76)
prob076 :: Int
prob076 = undefined

-- | Problem 77
-- [Prime summations](http://projecteuler.net/problem=77)
prob077 :: Int
prob077 = undefined


-- | Problem 78
-- [Coin partitions](http://projecteuler.net/problem=78)
prob078 :: Int
prob078 = undefined


-- | Problem 79
-- [Passcode derivation](http://projecteuler.net/problem=79)

-- http://en.wikipedia.org/wiki/Topological_sorting
-- global ordering from piecewise orderings

prob079 :: IO Int
prob079 = fmap (Utils.fromDigits . orderDigits . format079) data079

orderDigits :: [(Int,Int,Int)] -> [Int]
orderDigits xss = Graph.topSort g
  where
     edges = Set.toList . Set.fromList $ concatMap (\(a,b,c) -> [(a,b),(b,c)]) xss
     g = Graph.buildG (0, 9) edges


format079 :: [Int] -> [(Int,Int,Int)]
format079 = map toThreeDigits . IntSet.toList . IntSet.fromList
  where
    toThreeDigits n = case Utils.integerDigits n of
                          [a,b,c] -> (a,b,c)
                          _       -> error "wrong format!"

data079 :: IO [Int]
data079 = do
    r <- Wreq.get "https://projecteuler.net/project/resources/p079_keylog.txt"
    return . map read . lines . BLC8.unpack $ r ^. Wreq.responseBody




-- | Problem 80
-- [Square root digital expansion](http://projecteuler.net/problem=80)
prob080 :: Int
prob080 = sum . map hundredDigitSumOfSqrt
              . filter isNotSquared $ [1..100]
  where
    isNotSquared x = x `notElem` map (^2) [1..10]

-- The choice of 110 decimal places is to avoid a rounding error like 0.099999 -> 0.10,
-- which gave me a nasty bug and lots of headache.
hundredDigitSumOfSqrt :: Int -> Int
hundredDigitSumOfSqrt = sum . take 100
                            . map Char.digitToInt
                            . filter Char.isDigit
                            . CReal.showCReal 110
                            . sqrt . fromIntegral



-- Interface

-- select :: Int -> IO Int
-- select 79 = prob079
-- select n = return $ [prob071, prob072, prob073, prob074, prob075,
--                      prob076, prob077,       0,       0, prob080] !! (n - 71)

main :: IO ()
-- main = getArgs >>= return . read . head >>= select >>= print

main = prob079 >>= print
