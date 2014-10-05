{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens               ((^.))
import           Control.Applicative        ((<$>),(<*>))
import           Data.Vector                ((!))
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.Char                  as Char
import qualified Data.IntSet                as IntSet
import qualified Data.List                  as List
import qualified Data.Graph                 as Graph
import qualified Data.Number.CReal          as CReal
import qualified Data.Ratio                 as Ratio
import qualified Data.Set                   as Set
import qualified Data.Vector                as Vector
import qualified Network.Wreq               as Wreq
import           System.Environment         (getArgs)
import qualified Utils


-- | Problem 71
-- [Ordered fractions](http://projecteuler.net/problem=71)
-- Find the numerator of the fraction n/d for d<1000000 immediately to the left of 3/7.

prob071 :: Int
prob071 = Ratio.numerator $ getLeftOf (3 Ratio.% 7) 1000000

getLeftOf :: Ratio.Ratio Int -> Int -> Ratio.Ratio Int
getLeftOf frac d = frac - minimum deltas
  where
    lower = frac * (1 -  1 Ratio.% d)
    upper = frac * (1 - (1 Ratio.% d) / 1000)
    candidates = filter (\n -> ceiling (fromIntegral n * lower)
                              <= floor (fromIntegral n * upper)) [1..d]
    deltas = [frac - floor (upper * fromIntegral i) Ratio.% i | i <- candidates]



-- | Problem 72
-- [Counting fractions](http://projecteuler.net/problem=72)
--
prob072 :: Int
prob072 = sum $ map countFractions [2..1000000]

countFractions :: Int -> Int
countFractions d = d - 1 - tmp
  where
    distinctPrimes = map fst (Utils.factorInteger d)
    sign i = if odd i then id else negate
    term i = sign i . sum $ map (\xs -> -1 + d `div` product xs)
                                (Utils.combinations i distinctPrimes)
    tmp = sum $ map term [1..length distinctPrimes]



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
-- To speed up further, I should introduce a IntSet called knowledge
-- and put Int elements in repeating sequence into it.
-- The `knowledge` helps truncating unecessary computations in nonRepSequence.
-- Data.List.mapAccumL would be useful.
prob074 :: Int
prob074 = length $ filter (\n -> chainLength n == 60) [1..999999]

chainLength :: Int -> Int
chainLength = length . nonRepSequence . iterate digitFactMap

nonRepSequence :: Eq a => [a] -> [a]
nonRepSequence = npl []
  where
    npl stack (x:xs)
      | x `elem` stack = stack
      | otherwise      = npl (x:stack) xs
    npl stack   []     = stack

digitFactMap :: Int -> Int
digitFactMap = sum . map fact . Utils.integerDigits

fact :: Int -> Int
fact n = product [1..n]



-- | Problem 75
-- [Singular integer right triangles](http://projecteuler.net/problem=75)
prob075 :: Int
prob075 = length . filter (\(_, cnt) -> cnt == 1)
                 . Utils.frequencies
                 . concatMap (\x -> [x,2*x..maxL])
                 . map sum $ ppt maxL
  where maxL = 1500000

-- primitive pythagorean triangles
ppt :: Int -> [[Int]]
ppt maxL = concat . takeWhile (not . null)
                  . iterate (filter (\xs -> sum xs <= maxL) . dig)
                  $ [[3,4,5]]

dig :: [[Int]] -> [[Int]]
dig troikas = matrixMult <$> [matA, matB, matC] <*> troikas
  where
    matA = [[1,-2,2],[2,-1,2],[2,-2,3]]
    matB = [[1,2,2],[2,1,2],[2,2,3]]
    matC = [[-1,2,2],[-2,1,2],[-2,2,3]]


matrixMult :: Num a => [[a]] -> [a] -> [a]
matrixMult mat v = map (sum . zipWith (*) v) mat



-- | Problem 76
-- [Counting summations](http://projecteuler.net/problem=76)
--
-- (-1) is because single term (100 = 100) is excluded
prob076 :: Int
prob076 = partitionsP 100 - 1

-- Euler's pentagonal number theorem with memoisation
-- Note: This can easily overflows
partitionsP :: Int -> Int
partitionsP = (map p [0..] !!)
  where
    p 0 = 1
    p n = sum [sign m $ partitionsP (n-k) | (m, k) <- takeWhile (\(_, k) -> k <= n) pentaPairs]
    sign m = if odd m then id else negate



-- | Problem 77
-- [Prime summations](http://projecteuler.net/problem=77)
prob077 :: Int
prob077 = head $ filter (\n -> primePartitions n > 5000) [1..]

primePartitions :: Int -> Int
primePartitions n = integerPartitions n xs
  where xs = reverse $ Utils.primesTo n

-- [TODO] Rewrite in tail recursive form?
integerPartitions :: Int -> [Int] -> Int
integerPartitions 0 _      = 1
integerPartitions _ []     = 0
integerPartitions n zs@(x:xs)
  | n < x     = integerPartitions n xs
  | otherwise = integerPartitions (n - x) zs + integerPartitions n xs



-- | Problem 78
-- [Coin partitions](http://projecteuler.net/problem=78)
-- Find the least value of n for which p(n), partition function, is divisible by one million.

prob078 :: Int
prob078 = head $ filter (\n -> partitionsMod ! n == 0) [1..]

-- Vector implementation
partitionsMod :: Vector.Vector Int
partitionsMod = Vector.generate nMax p
  where
    nMax = 100000
    sign m = if odd m then id else negate
    p 0 = 1
    p n = sum [sign m $ partitionsMod ! (n-k) |
                    (m, k) <- takeWhile (\(_, k) -> k <= n) pentaPairs] `mod` 1000000


-- Euler's pentagonal number theorem (partition function) in modulo system
-- together with memoization

-- prob078 = head $ filter (\n -> memP n == 0) [1..]
-- memP :: Int -> Int
-- memP = (map p [0..] !!)
--   where
--     p 0 = 1
--     p n = sum [sign m $ memP (n-k) |
--                   (m, k) <- takeWhile (\(_, k) -> k <= n) pentaPairs] `mod` 1000000
--     sign m = if odd m then id else negate

pentaPairs :: [(Int, Int)]
pentaPairs = Utils.roundRobin [xs, ys]
  where
    xs = zip [1..]     (scanl1 (+) [1,4..])
    ys = zip [-1,-2..] (scanl1 (+) [2,5..])




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
format079 = map toTuple . IntSet.toList . IntSet.fromList
  where
    toTuple n = case Utils.integerDigits n of
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
    isNotSquared :: Int -> Bool
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
select :: Int -> IO Int
select 79 = prob079
select n = return $ [prob071, prob072, prob073, prob074, prob075,
                     prob076, prob077, prob078,       0, prob080] !! (n - 71)


main :: IO ()
main = getArgs >>= return . read . head >>= select >>= print
