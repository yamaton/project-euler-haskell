module Utils where

{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}  

import Data.List (unfoldr, foldl')
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (evalState, get, put)
import Control.Monad (when, forM_, replicateM)
import Data.Array.ST (newArray, readArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, assocs)
import Data.Hashable (Hashable)
import qualified Numeric
import qualified Data.Char    as Char
import qualified Data.Ord     as Ord
import qualified Data.List    as List
import qualified Data.Map     as Map
import qualified Data.HashSet as HashSet

--------------------------------------------------------------
--              List Utilities
--------------------------------------------------------------

{- | partition' n d xs 
     generates sublists of length n with offset d.
     [This version cannot handle infinite series xs!]

>>> partition' 4 2 [1..7]
[[1,2,3,4],[3,4,5,6]]
-}
partition' :: Int -> Int -> [a] -> [[a]]
partition' n d xs = helper (length xs) xs
  where helper l xs
          | l < n     = []
          | otherwise = take n xs : helper (l-d) (drop d xs)


{- | partition n d xs 
     generates sublists of length n with offset d.
     [This nicely handles infinite series xs!]
        
    `groupOf n d xs` might be better name for the function,
     especially because List.partition already exists in Haskell.

>>> partition 4 2 [1..7]
[[1,2,3,4],[3,4,5,6]]

>>> partition 2 4 [1..7]
[[1,2],[5,6]]

>>> partition 2 4 [1..6]
[[1,2],[5,6]]
        
-}
partition :: Int -> Int -> [a] -> [[a]]
partition _ _ [] = []
partition n d xs
  | length (take n xs) < n = [] 
  | otherwise              = (take n xs) : partition n d (drop d xs)

  
{- | round robin

>>> roundRobin ["abc", "d", "ef"]
"adebfc"

-}
roundRobin :: [[a]] -> [a]
roundRobin [] = []
roundRobin xs = map head ys ++ roundRobin (map tail ys)
  where ys = filter (not . null) xs


{- | Split string with specified char 

>>> splitOn ',' "aa,bc,cd,e"
["aa","bc","cd","e"]

-}
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s = case dropWhile (== c) s of
  [] -> []
  s' -> w : splitOn c s''
    where (w, s'') = break (== c) s'


{- | JoinList elements in [String] by inserting String in between them

>>> joinList ";" ["a", "bc", "  ", "dd  f"]
"a;bc;  ;dd  f"

-}
joinList :: [a] -> [[a]] -> [a]
joinList = List.intercalate


{- | partialPermutations n xs 
     generates permutaitons of length n

>>> partialPermutations 2 [1..4]
[[1,2],[2,1],[1,3],[3,1],[1,4],[4,1],[2,3],[3,2],[2,4],[4,2],[3,4],[4,3]]

-}
partialPermutations :: Int -> [a] -> [[a]]
partialPermutations n xs = concatMap List.permutations $ combinations n xs


{- | combinations n xs
     generates combinations of length n

>>> combinations 2 [1..4]
[[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]

-}
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


{- | Equivalent to `combinations_with_relacement` in itertools of Python,

>>> combinationsWithReplacement 2 "abc"
["aa","ab","ac","bb","bc","cc"]

-}
combinationsWithReplacement :: (Ord a, Hashable a) => Int -> [a] -> [[a]]
combinationsWithReplacement n = 
  List.sort . HashSet.toList . HashSet.fromList . map List.sort . replicateM n


{- | Equivalent to Tuple in Mathematica

>>> tuples 2 "abc"
["aa","ab","ac","ba","bb","bc","ca","cb","cc"]

-}
tuples :: Int -> [a] -> [[a]]
tuples = replicateM


{- | Frequencies (occurrences) of the element in list
http://stackoverflow.com/questions/7108559/how-to-find-the-frequency-of-characters-in-a-string-in-haskell

>>> tally "aaaddbcdabbbaf"
[('a',5),('b',4),('c',1),('d',3),('f',1)]

-}
tally :: Ord a => [a] -> [(a, Int)]
tally = Map.toList . Map.fromListWith (+) . map (\x -> (x, 1))


frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = tally


{- | Get the most frequently appearing element from a list

>>> mostFrequent "akakakdkdkbkckdkabkbkbkakfkk"
'k'

-}
mostFrequent :: Ord a => [a] -> a
mostFrequent = fst . List.maximumBy (Ord.comparing snd) . frequencies


{- | Cartesian product

>>> cartesianProduct [[1,2,3], [7,8], [9]]
[[1,7,9],[1,8,9],[2,7,9],[2,8,9],[3,7,9],[3,8,9]]

-}
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct = foldr (\xs acc -> (:) <$> xs <*> acc) [[]]


{- | Conunt elements in list

>>> count 'a' "afdadaaaa"
6

-}
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
-- count x = foldr (\y acc -> if y == x then acc + 1 else acc) 0


{- | Take Every n elements from a list

>>> takeEvery 10 [1..55]
[10,20,30,40,50]

-}
takeEvery :: Int -> [a] -> [a]
takeEvery n xs = 
  case drop (n - 1) xs of
    []     -> []
    (y:ys) -> y : takeEvery n ys 


{- | reshape list into list of list

>>> reshapeBy 3 [1..10]
[[1,2,3],[4,5,6],[7,8,9],[10]]

-}
reshapeBy :: Int -> [a] -> [[a]]
reshapeBy n xs = 
  case splitAt n xs of
    ([], _)  -> []
    (ys,zs)  -> ys : reshapeBy n zs



--------------------------------------------------------------
--              Number Theory Utilities
--------------------------------------------------------------

{- | Fibonacci sequence

>>> take 10 $ fibonacciSequence 
[1,1,2,3,5,8,13,21,34,55]

-}
fibonacciSequence :: Integral a => [a]
fibonacciSequence = unfoldr (\(a, b) -> Just (a, (b, a + b))) (1, 1)


{- | Integer to digits

>>> integerDigits 41531
[4,1,5,3,1]

-}
integerDigits :: (Show a, Integral a) => a -> [Int]
integerDigits = map Char.digitToInt . show

-- integerDigits :: (Integral a) => a -> [a]
-- integerDigits = reverse . map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)


{- | Digits to integer

>>> fromDigits [1,6,1,5,2]
16152

>>> fromDigits [0,1,2,7]
127

prop> \n -> (n > 0) ==> n == fromDigits (integerDigits (n :: Int))

-}
fromDigits :: (Read a, Integral a) => [Int] -> a
fromDigits xs = read $ concatMap show xs


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


{- | Sieve of Atkin
http://en.wikipedia.org/wiki/Sieve_of_Atkin    
    
-}

{- | Generate first n primes 

Rosser's theorem is used to get an upper bound:
For n-th prime number P(n), for n > 6
log(n) + log(log(n)) - 1 < P(n)/n < log(n) + log(log(n))  
http://en.wikipedia.org/wiki/Prime_number_theorem

>>> primes 10
[2,3,5,7,11,13,17,19,23,29]

-}
primes :: Int -> [Int]
primes n
  | n < 6     = take n [2, 3, 5, 7, 11]
  | otherwise = take n [i | (i, True) <- assocs $ sieve ub]
    where 
      x = fromIntegral n
      ub = floor $ x * (log x + log (log x))


{- | Generate primes less than or equal to n

>>> primesTo 20
[2,3,5,7,11,13,17,19]

-}
primesTo :: Int -> [Int]
primesTo n
  | n < 2     = []
  | otherwise = [i | (i, True) <- assocs $ sieve n]


{- | Return factors by a list of (prime, exponent)

>>> factorInteger 24
[(2,3),(3,1)]

>>> factorInteger 141
[(3,1),(47,1)]

>>> factorInteger 151
[(151,1)]

>>> factorInteger 5
[(5,1)]

-}
factorInteger :: Int -> [(Int, Int)]
factorInteger 0 = [(0, 1)]
factorInteger 1 = [(1, 1)]
factorInteger n = frequencies $ factor n
  where
    ps = primesTo . round . sqrt . fromIntegral $ n
    factor :: Int -> [Int]
    factor 1 = []
    factor p = k : factor (p `div` k)
      where 
        ds = dropWhile (\q -> p `mod` q /= 0) ps
        k = if null ds then p else head ds


{- | Find a list of divisors of an integer
   !!!!!!!! VERY SLOW !!!!!!!!

>>> divisors 24
[1,2,3,4,6,8,12,24]

>>> divisors 141
[1,3,47,141]

>>> divisors 151
[1,151]

-}
divisors :: Int -> [Int]
divisors 1 = [1]
divisors n = List.sort [product xs | xs <- cartesianProduct factors]
  where factors = [ map (n^) [0..pow] | (n, pow) <- factorInteger n ]


{- | Number of divisors of n
     d(n) in http://en.wikipedia.org/wiki/Table_of_divisors

>>> divisorsCount 72
12

>>> divisorsCount 378
16

>>> divisorsCount 256
9

 -}
divisorsCount :: Int -> Int
divisorsCount n = 2 + 2 * (length $ filter (\k -> n `mod` k == 0) [2..m]) - a
  where
    m = floor . sqrt . fromIntegral $ n
    a = if m * m == n then 1 else 0


{- | Sum of divisors of n

>>> divisorsSum 72
195

>>> divisorsSum 378
960  

>>> divisorsSum 256
511
    
-}
divisorsSum :: Int -> Int
divisorsSum n = (1 + n) + (sum . map (\k -> k + n `div` k ) . filter (\k -> n `mod` k == 0) $ [2..p]) - adjuster
  where
    p = floor . sqrt . fromIntegral $ n
    adjuster = if p * p == n then p else 0


{- | Check if integer is palindrome: O(n^2) at worst

>>> isPalindrome 3
True

>>> isPalindrome 1051501
True

>>> isPalindrome 100011
False

-}
isPalindrome :: (Integral a, Show a) => a -> Bool
isPalindrome n = helper (show n)
  where 
    helper [] = True
    helper [_] = True
    helper (x:xs) = (x == last xs) && helper (init xs)


{- | Check if integer is prime number

>>> isPrime 15161
True

>>> isPrime 15163
False

-}
isPrime :: Int -> Bool
isPrime 2 = True
isPrime n
  | n < 2     = False
  | even n    = False
  | otherwise = all ((/= 0) . mod n) [3, 5 .. ub]
  where ub = (floor . sqrt . fromIntegral) n


{- | Check if an integer p is divisible by another q

>>> isDivisible 10 3
False

>>> isDivisible 10 5
True

-}
isDivisible :: Integral a => a -> a -> Bool
isDivisible p q = mod p q == 0


{- | Binomial coefficient

>>> binomial 10 3
120

>> binomial 6 4
15

-}
binomial :: Integral a => a -> a -> a
binomial n k 
  | k < 0     = 0
  | k > n     = 0
  | otherwise = foldl' (\z i -> z * (n-i+1) `div` i) 1 [1..min k (n-k)]



--------------------------------------------------------------
--              Binary and Hexadecimal
--------------------------------------------------------------
                
{- | From integer to binary string  

>>> intToBin 100
"1100100"

-}
intToBin :: Int -> String
intToBin n = Numeric.showIntAtBase 2 Char.intToDigit n ""


{- | From binary string string to Int

>>> binToInt "1100100"
100

-}
binToInt :: String -> Int
binToInt xs = sum $ zipWith (*) digits pows
  where 
    n = length xs
    pows = map (2^) $ reverse [0 .. n - 1]
    digits = map (read . (:[])) xs


{- | Hex string to integer

>>> hexToInt "ffffff"
16777215

>>> hexToInt "Ab"
171

-}
hexToInt :: String -> Int
hexToInt = fst . head . Numeric.readHex


-- taken from McBride-Paterson "Applicative Programming with Effects"
{- | Transpose matrix (a list of a list)
>>> transpose [[1,2,3],[4,5,6]]
[[1,4],[2,5],[3,6]]

-}
-- -> Use List.transpose 
transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)

