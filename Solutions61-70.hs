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
import qualified Data.Text.Lazy          as TextLazy
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Data.ByteString.Lazy    as BL
import           System.Environment (getArgs)
import qualified Utils

-- | Problem 61
-- [Cyclical figurate numbers](http://projecteuler.net/problem=61)

-- prob061 :: Int
prob061 = sum . head $ concatMap search061 xsss
  where xsss = List.permutations [s3, s4, s5, s6, s7, s8]
        take4dig = takeWhile (< 10000). dropWhile (< 1000)
        s3 = take4dig $ scanl1 (+) [1,2..]
        s4 = take4dig $ scanl1 (+) [1,3..]
        s5 = take4dig $ scanl1 (+) [1,4..]
        s6 = take4dig $ scanl1 (+) [1,5..]
        s7 = take4dig $ scanl1 (+) [1,6..]
        s8 = take4dig $ scanl1 (+) [1,7..]

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
-- [](http://projecteuler.net/problem=63)
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
-- [](http://projecteuler.net/problem=64)

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



-- Not used in the problem
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



-- | Problem 65
-- [](http://projecteuler.net/problem=65)
prob065 :: Int
prob065 = sum . Utils.integerDigits . Ratio.numerator $ fromContinuedFraction 2 xs
  where xs = take 99 $ Utils.roundRobin [repeat 1, [2,4..], repeat 1]

fromContinuedFraction :: Int -> [Int] -> Rational
fromContinuedFraction n xs = fromIntegral n + go xs
  where
    go :: [Int] -> Rational
    go []     = 0
    go (x:xs) = 1 / (fromIntegral x + go xs)



-- | Problem 66
-- [](http://projecteuler.net/problem=66)
prob066 :: Int
prob066 = undefined

-- | Problem 67
-- [](http://projecteuler.net/problem=67)
prob067 :: Int
prob067 = undefined


-- | Problem 68
-- [](http://projecteuler.net/problem=68)
prob068 :: Int
prob068 = undefined


-- | Problem 69
-- [](http://projecteuler.net/problem=69)
prob069 :: Int
prob069 = undefined

-- | Problem 70
-- [](http://projecteuler.net/problem=70)
prob070 :: Int
prob070 = undefined


-- Interface

-- select :: Int -> IO Int
-- select n = return $ [prob061, prob062, prob063, prob064, prob065,
--                      prob066, prob067, prob068, prob069, prob070] !! (n - 51)

main :: IO ()
-- main = getArgs >>= return . read . head >>= select >>= print

-- main = print $ map period [1..100]
main = print $ prob065