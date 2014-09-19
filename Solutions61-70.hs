{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens ((^.))
import           Data.List ((\\))
import qualified Data.List   as List
import qualified Data.Char   as Char
import qualified Data.Set    as Set
import qualified Data.IntSet as IntSet
import qualified Data.Ratio  as Ratio
import qualified Control.Monad as Monad
import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Lazy.Char8  as BLC8
import           System.Environment (getArgs)
import qualified Utils

-- | Problem 61
-- [Cyclical figurate numbers](http://projecteuler.net/problem=61)

prob061 :: Int
prob061 = sum . head $ concatMap search061 xsss
  where xsss = List.permutations yss
        take4dig = takeWhile (< 10000). dropWhile (< 1000)
        yss = map (\n -> take4dig $ scanl1 (+) [1,n..]) [2,3,4,5,6,7]

-- Return list of cycling, linkabale ints
search061 :: [[Int]] -> [[Int]]
search061 xss = filter isCyclic $ foldr step seeds (init xss)
  where
    seeds = List.transpose [last xss]
    step :: [Int] -> [[Int]] -> [[Int]]
    step ns yss = [ n:ys | n <- ns, ys <- yss, isLinkable n (head ys)]

-- |
-- >>> isLinkable 5213 1346
-- True
-- >>> isLinkable 1346 5213
-- False
isLinkable :: Int -> Int -> Bool
isLinkable x y = drop 2 digX == take 2 digY
  where [digX, digY] = map Utils.integerDigits [x, y]

-- |
-- >>> isCyclic [1234, 3456, 5612]
-- True
-- >>> isCyclic [1234, 5612]
-- True
isCyclic :: [Int] -> Bool
isCyclic [] = False
isCyclic xs = isLinkable (last xs) (head xs)


-- | Problem 62
-- [Cubic permutations](http://projecteuler.net/problem=62)
-- Find the smallest cube for which exactly five permutations of its digits are cube.

prob062 :: Int
prob062 = head [ minimum (concat xss) | n <- [1..], let xss = findPermsFromCubes 5 n, (not . null) xss ]

-- | Return all cubes of the specified digit length.
allCubes :: Int -> [Int]
allCubes n = [ x | a <- [start..end], let x = a^3]
  where start = ceiling $ 10**((fromIntegral n-1) /3)
        end   = floor   $ 10**(fromIntegral n /3)


-- | Find (exactly) k cubes related by digit permutations among the n ndigit numbers
-- 
-- >>> findPermsFromCubes 3 8
-- [[41063625,56623104,66430125]]
--
findPermsFromCubes :: Int -> Int -> [[Int]]
findPermsFromCubes k n = [[x | x <- xs, x `isPermOf` ds] | ds <- dss ]
  where
    xs = allCubes n
    isPermOf :: Int -> [Int] -> Bool
    isPermOf p digs = digs == List.sort (Utils.integerDigits p)
    freqs = Utils.frequencies . map (List.sort. Utils.integerDigits) $ xs
    dss = [ ds | (ds, count) <- freqs, count == k ]



-- | Problem 63
-- [Powerful digit counts](http://projecteuler.net/problem=63)
-- How many n-digit positive integers exist which are also an nth power?

-- n-digit integer that is nth power is represented by
-- 10^(n-1) <= x^n < 10^n 
-- which gives the lower and upper bound of x
-- 10^((n-1)/n)) <= x < 10
-- This equality also gives the upper bound of n
-- n <= 1 / (1 - log10 9) ~=~ 21.85

prob063 :: Int
prob063 = length [x^n | n <- [1..21], let p = fromIntegral n,
                        x <- [ceiling (10**((p-1)/p)) .. 9]]


-- | Problem 64
-- [Odd period square roots](http://projecteuler.net/problem=64)

-- This is a rough hack and contains lots of problematic coding and approaches.
prob064 :: Int
prob064 = length [1 | n <- [1..10000], odd (period n)]


-- The iterative map (a,b,c) -> (a',b',c') is actuall the iteration of the form  p -> p' where
--   p = (a √n + b)/c

period :: Int -> Int
period n = detectPeriod . tail . takeWhile (\(a, b, c) -> c /= 0) $ iterate (stepMod n) (1, 0, 1)
  where
    stepMod :: Int -> (Int, Int, Int) -> (Int, Int, Int)
    stepMod n (a, b, c) = (aNext, bNext, cNext)
      where 
        k = floor $ (fromIntegral a * sqrt (fromIntegral n) + fromIntegral b) / fromIntegral c
        aTemp = a * c
        bTemp = c * (c * k - b)
        cTemp = a^2 *n - (b - c*k)^2
        factor = foldr1 gcd [aTemp, bTemp, cTemp]
        [aNext, bNext, cNext] = map (`div` factor) [aTemp, bTemp, cTemp]
    
    detectPeriod :: (Eq a) => [a] -> Int
    detectPeriod []     = 0
    detectPeriod (x:xs) = 1 + length (takeWhile (/= x) xs)



-- | Problem 65
-- [Convergents of e](http://projecteuler.net/problem=65)
prob065 :: Int
prob065 = sum . Utils.integerDigits . Ratio.numerator $ fromContinuedFraction (2:xs)
  where xs = take 99 $ Utils.roundRobin [repeat 1, [2,4..], repeat 1]

fromContinuedFraction :: [Int] -> Rational
fromContinuedFraction (n:xs) = fromIntegral n + go xs
  where
    go :: [Int] -> Rational
    go []     = 0
    go (x:xs) = 1 / (fromIntegral x + go xs)



-- | Problem 66
-- [Diophantine equation](http://projecteuler.net/problem=66)
prob066 :: Int
prob066 = snd $ maximum [(pellsEquation d, d) | d <- [1..1000], isNotSquare d]

isNotSquare :: Int -> Bool
isNotSquare n = p * p /= n
  where p = floor . sqrt $ fromIntegral n


-- Return (x, y), with the minimum x, for given d satisfying the Pell's equation
--    x^2 - d * y^2 == 1
-- 
-- >>> pellsEquation 5
-- (9, 4)
-- >>> pellsEquation 7 
-- (8, 3)
--
pellsEquation :: Int -> (Integer, Integer)
pellsEquation d = head . filter isGood . map toPair $ convergents d
  where
    toPair :: Rational -> (Integer, Integer)
    toPair r = (Ratio.numerator r, Ratio.denominator r)
    isGood :: (Integer, Integer) -> Bool
    isGood (x,y) = x*x - fromIntegral d * y*y == 1

convergents :: Int -> [Rational]
convergents =  map fromContinuedFraction . tail . List.inits . sqrtToContinuedFraction

sqrtToContinuedFraction :: Int -> [Int]
sqrtToContinuedFraction n = List.unfoldr (step n) (1, 0, 1) 
  where
    step :: Int -> (Int, Int, Int) -> Maybe (Int, (Int, Int, Int))
    step n (a, b, c)
      | c == 0    = Nothing
      | otherwise = Just (k, (aNext, bNext, cNext))
      where 
        k = floor $ (fromIntegral a * sqrt (fromIntegral n) + fromIntegral b) / fromIntegral c
        aTemp = a * c
        bTemp = c * (c * k - b)
        cTemp = a^2 *n - (b - c*k)^2
        factor = foldr1 gcd [aTemp, bTemp, cTemp]
        [aNext, bNext, cNext] = map (`div` factor) [aTemp, bTemp, cTemp]



-- | Problem 67
-- [Maximum path sum II](http://projecteuler.net/problem=67)
prob067 :: IO Int
prob067 = fmap findMax data067

data067 :: IO [[Int]]
data067 = do
    r <- Wreq.get "https://projecteuler.net/project/resources/p067_triangle.txt"
    return $ format067 $ r ^. Wreq.responseBody

format067 :: BLC8.ByteString -> [[Int]]
format067 = map (map read . words) . lines . BLC8.unpack

-- from prob018
findMax  :: [[Int]] -> Int
findMax triangle = head $ foldr1 f triangle
  where f xs ys = zipWith3 (\a b c -> a + max b c) xs (tail ys) (init ys)




-- | Problem 68
-- [Magic 5-gon ring](http://projecteuler.net/problem=68)
prob068 :: Int
prob068 = maximum . map Utils.fromDigits . filter isValid $ prepareDigits

isValid :: [Int] -> Bool
isValid = all (\ds -> sum ds == 14) . Utils.partition 3 3

prepareDigits :: [[Int]]
prepareDigits = [Utils.roundRobin [x, y, (tail y ++ [head y])] | 
                    x <- outers, y <- inners]
  where 
    outers = map (6:) $ List.permutations [7..10]
    inners = List.permutations [1..5]


-- | Problem 69
-- [Totient maximum](http://projecteuler.net/problem=69)
prob069 :: Int
prob069 = last . takeWhile (< 1000000) . scanl1 (*) $ Utils.primes 100



-- | Problem 70
-- [Totient permutation](http://projecteuler.net/problem=70)
prob070 :: Int
prob070 = undefined




-- Interface

-- select :: Int -> IO Int
-- select 67 = prob067
-- select n = return $ [prob061, prob062, prob063, prob064, prob065,
--                      prob066,       0, prob068, prob069, prob070] !! (n - 61)


main :: IO ()
-- main = getArgs >>= return . read . head >>= select >>= print

-- main = print $ map period [1..100]
main = print $ prob061