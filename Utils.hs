module Utils where

{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}  

import Data.Char (intToDigit)
import Numeric (showIntAtBase, readHex)
import Data.List (intercalate, nub, sort, permutations, unfoldr, foldl')
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (evalState, get, put)
import Data.Map (fromListWith, toList)
import Test.QuickCheck
import Control.Monad (when, forM_, replicateM)
import Data.Array.ST (newArray, readArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, assocs)

-- |  Round-Robin 
-- >>> roundRobin ["abc", "d", "ef"]
-- "adebfc"
roundRobin :: [[a]] -> [a]
roundRobin [] = []
roundRobin xs = map head xs ++ roundRobin (filter (not . null) (map tail xs))


-- | Split string with specified char 
-- >>> splitOn ',' "aa,bc,cd,e"
-- ["aa","bc","cd","e"]
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s = case dropWhile (== c) s of
  [] -> []
  s' -> w : splitOn c s''
    where (w, s'') = break (== c) s'


-- | JoinList elements in [String] by inserting String in between them
-- >>> joinList ";" ["a", "bc", "  ", "dd  f"]
-- "a;bc;  ;dd  f"
joinList :: [a] -> [[a]] -> [a]
joinList = intercalate


-- | Partial Permutations
-- >>> partialPermutations 2 [1..4]
-- [[1,2],[2,1],[1,3],[3,1],[1,4],[4,1],[2,3],[3,2],[2,4],[4,2],[3,4],[4,3]]
partialPermutations :: Int -> [a] -> [[a]]
partialPermutations n xs = concatMap permutations $ combinations n xs


-- | Combinations 
-- >>> combinations 2 [1..4]
-- [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations 1 xs = map (:[]) xs 
combinations n xs = helper n (length xs) xs    
  where
    helper k l ys@(z:zs)        
      | k < l     = map (z:) (combinations (k-1) zs) ++ combinations k zs
      | k == l    = [ys]
      | otherwise = []


-- | Equivalent to `combinations_with_relacement` in itertools of Python,
-- >>> combinationsWithReplacement 2 "abc"
-- ["aa","ab","ac","bb","bc","cc"]
combinationsWithReplacement :: Ord a => Int -> [a] -> [[a]]
combinationsWithReplacement n xs = nub . map sort $ replicateM n xs


-- | Equivalent to the command in Mathematica
-- >>> tuples 2 "abc"
-- ["aa","ab","ac","ba","bb","bc","ca","cb","cc"]
tuples :: Int -> [a] -> [[a]]
tuples = replicateM


-- | Frequency (occurrence) of element
-- http://stackoverflow.com/questions/7108559/how-to-find-the-frequency-of-characters-in-a-string-in-haskell
--
-- >>> tally "aaaddbcdabbbaf"
-- [('a',5),('b',4),('c',1),('d',3),('f',1)]
tally :: Ord a => [a] -> [(a, Int)]
tally xs = toList $ fromListWith (+) [(x, 1)| x <- xs]


-- Frequencies (yet another implementation of tally)
--frequencies :: Ord a => [a] -> [(a, Int)]
--frequencies xs = drop
--  x <- group $ sort xs
--  return (head x, length x)


-- | Fibonacci sequence
-- >>> take 10 $ fibonacciSeq 
-- [1,2,3,5,8,13,21,34,55,89]
fibonacciSeq :: [Int]
fibonacciSeq = unfoldr (\(a, b) -> Just (a, (b, a + b))) (1, 2)


-- | Integer to digits
-- >>> integerDigits 41531
-- [4,1,5,3,1]
integerDigits :: Int -> [Int]
integerDigits = reverse . map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10) 


-- | Digits to integer
-- >>> fromDigits [1,6,1,5,2]
-- 16152
fromDigits :: [Int] -> Int
fromDigits xs = read $ concatMap show xs

---- For quickCheck...
--prop_integer :: Int -> Bool
--prop_integer n       = 
--  not (n < 0) ==> 
--    n == fromDigits $ integerDigits n

--prop_digits :: [Int] -> Bool
--prop_digits xs  =  
--    xs == integerDigits $ fromDigits xs


-- |
-- >>> cartesianProduct [[1,2,3], [7,8], [9]]
-- [[1,7,9],[1,8,9],[2,7,9],[2,8,9],[3,7,9],[3,8,9]]
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct = foldr (\xs acc -> (:) <$> xs <*> acc) [[]]


-- | Eratosthenes sieve
sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
    let maxP = floor . sqrt $ fromIntegral n
    sieveTF <- newArray (2, n) True 
    forM_ [2..maxP] $ \p -> do
      isPrime <- readArray sieveTF p
      when isPrime $ do
        forM_ [p*p, p*p+p .. n] $ \q -> do
          writeArray sieveTF q False
    return sieveTF


-- | Rosser's theorem is used to get an upper bound:
-- For n-th prime number P(n), for n > 6
-- log(n) + log(log(n)) - 1 < P(n)/n < log(n) + log(log(n))  
-- http://en.wikipedia.org/wiki/Prime_number_theorem
-- >>> primes 10
-- [2,3,5,7,11,13,17,19,23,29]
primes :: Int -> [Int]
primes n
  | n < 6     = take n [2, 3, 5, 7, 11]
  | otherwise = take n [i | (i, True) <- assocs $ sieve ub]
    where 
      x = fromIntegral n
      ub = floor $ x * (log x + log (log x))


-- | 
-- >>> primesTo 20
-- [2,3,5,7,11,13,17,19]
primesTo :: Int -> [Int]
primesTo n
  | n < 2     = []
  | otherwise = [i | (i, True) <- assocs $ sieve n]


-- | 
-- >>> factorInteger 24
-- [(2,3),(3,1)]
-- >>> factorInteger 141
-- [(3,1),(47,1)]
-- >>> factorInteger 151
-- [(151,1)]
-- >>> factorInteger 5
-- [(5,1)]
factorInteger :: Int -> [(Int, Int)]
factorInteger 0 = [(0, 1)]
factorInteger 1 = [(1, 1)]
factorInteger n = tally $ factor n
  where
    ps = (primesTo . round . sqrt . fromIntegral) n
    factor 1 = []
    factor p = k : factor (p `div` k)
      where 
        ds = dropWhile (\q -> p `mod` q /= 0) ps
        k = if null ds then p else head ds

-- | 
-- >>> divisors 24
-- [1,2,3,4,6,8,12,24]
-- >>> divisors 141
-- [1,3,47,141]
-- >>> divisors 151
-- [1,151]
divisors :: Int -> [Int]
divisors 1 = [1]
divisors n = sort [product xs | xs <- cartesianProduct factors]
  where factors = [ map (n^) [0..pow] | (n, pow) <- factorInteger n ]


-- | Check if integer is palindrome
-- >>> isPalindrome 3
-- True
-- >>> isPalindrome 1051501
-- True
-- >>> isPalindrome 100011
-- False
isPalindrome :: Int -> Bool
isPalindrome n = isPalindrome' (show n)
  where 
    isPalindrome' [] = True
    isPalindrome' [_] = True
    isPalindrome' (x:xs) = (x == last xs) && isPalindrome' (init xs)


-- | Check if integer is prime number
-- >>> isPrime 15161
-- True
-- >>> isPrime 15163
-- False
isPrime :: Int -> Bool
isPrime 2 = True
isPrime n
  | n < 2     = False
  | even n    = False
  | otherwise = all ((/= 0) . mod n) [3, 5 .. ub]
  where ub = (floor . sqrt . fromIntegral) n

                
-- | Integer to binary string  
-- >>> intToBin 100
-- "1100100"
intToBin :: Int -> String
intToBin n = showIntAtBase 2 intToDigit n ""


-- | Binary string to Int
-- >>> binToInt "1100100"
-- 100
binToInt :: String -> Int
binToInt xs = sum $ zipWith (*) digits pows
  where 
    n = length xs
    pows = map (2^) $ reverse [0 .. n - 1]
    digits = map (read . (:[])) xs


-- | Hex string to integer
-- >>> hexToInt "ffffff"
-- 16777215
-- >>> hexToInt "Ab"
-- 171
hexToInt :: String -> Int
hexToInt = fst . head . readHex



-- | Conunt elements in list
-- >>> count 'a' "afdadaaaa"
-- 6
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)


-- | Take Every n elements from a list
-- >>> takeEvery 10 [1..55]
-- [10,20,30,40,50]
takeEvery :: Int -> [a] -> [a]
takeEvery n xs = 
  case drop (n - 1) xs of
    []     -> []
    (y:ys) -> y : takeEvery n ys 


-- | reshape list into list of list
-- >>> reshapeBy 3 [1..10]
-- [[1,2,3],[4,5,6],[7,8,9],[10]]
reshapeBy :: Int -> [a] -> [[a]]
reshapeBy n xs = 
  case splitAt n xs of
    ([], _)  -> []
    (ys,zs)  -> ys : reshapeBy n zs


-- | Check if an integer p is divisible by another q
-- >>> isDivisible 10 3
-- False
-- >>> isDivisible 10 5
-- True
isDivisible :: Integral a => a -> a -> Bool
isDivisible p q = mod p q == 0


-- | Binomial coefficient
binomial :: Integral a => a -> a -> a
binomial n k 
  | k < 0     = 0
  | k > n     = 0
  | otherwise = foldl' (\z i -> z * (n-i+1) `div` i) 1 [1..min k (n-k)]

-- | unfold function
-- From pp. 73 of Hutton "Programming in Haskell" 
-- 
-- use cases:
-- map f = unfold null (f . head) tail
-- iterate f = unfold (const True) id f

--unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> [b]
--unfold p h t x 
--  | p x          = []
--  |otherwise = h x : unfold p h t (t x)

