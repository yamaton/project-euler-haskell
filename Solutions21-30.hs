{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens ((^.))
import           Data.Char (ord)
import           Data.Text.Encoding as TE
import qualified Data.List as List
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           System.Environment (getArgs)
import qualified Utils as Utils


-- | Problem 21
-- [Amicable Numbers](http://projecteuler.net/problem=21)
-- Evaluate the sum of all the amicable numbers under 10000.

isAmicable :: Int -> Bool
isAmicable 1 = False
isAmicable n = (n /= m) && (n == divSum m)
  where divSum :: Int -> Int
        divSum x = Utils.divisorsSum x - x
        m = divSum n

prob021 :: Int
prob021 = sum . filter isAmicable $ [1..10000]


-- Problem 22
-- [Names scores](http://projecteuler.net/problem=22)
-- 
prob022 :: IO Int
prob022 = do
   r <- W.get "http://projecteuler.net/project/names.txt"
   let dat = format022 $ r ^. W.responseBody
   return $ totalScore dat

format022 :: BL.ByteString -> [String]
format022 = List.sort . Utils.splitOn ',' . filter (/= '\"') . T.unpack . TE.decodeLatin1 . B.concat . BL.toChunks

name2int :: String -> Int
name2int = sum . map alph2int
  where alph2int c = ord c - ord 'A' + 1

totalScore :: [String] -> Int
totalScore = sum . zipWith (\i s -> i * name2int s) [1..]


-- Problem 23
-- [Names scores](http://projecteuler.net/problem=23)
-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

isAbundant :: Int -> Bool
isAbundant n = Utils.divisorsSum n > 2 * n

abundantList :: [Int]
abundantList = filter isAbundant prob023data

prob023data :: [Int]
prob023data = [1..28123]

prob023 :: Int
prob023 = IS.foldr (+) 0 $ IS.difference (IS.fromList prob023data) (IS.fromList sumOfTwoAbundants)
    where sumOfTwoAbundants = [k | i <- abundantList, 
                                   j <- abundantList, 
                                   i <= j, 
                                   let k = i + j, 
                                   k <= 28123]


-- Problem 24
-- [Lexicographic permutations](http://projecteuler.net/problem=24)

-- [Note] List.permutations is tad slow.
prob024 :: Int
prob024 = Utils.fromDigits (x !! 1000000)
    where x = List.sort $ List.permutations [0..9]


-- Problem 25
-- [1000-digit Fibonacci number](http://projecteuler.net/problem=25)
-- Find the first term in the Fibonacci sequence to contain 1000 digits.

digitLen :: (Integral a, Show a) => a -> Int
digitLen = length . Utils.integerDigits 

prob025 :: Int
prob025 = 1 + (length $ takeWhile (\n -> digitLen n < 1000) Utils.fibonacciSequence)



-- Problem 26
-- [Reciprocal cycles](http://projecteuler.net/problem=26)
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

recurringCycle :: Int -> Int
recurringCycle n = if null xs then 0 else fromInteger (head xs)
  where p  = toInteger n
        xs = dropWhile (\i -> (10 ^ i - 1) `mod` p > 0) $ [1..p]

prob026 :: Int
prob026 = snd . maximum . map (\n -> (recurringCycle n, n)) . drop 3 $ Utils.primesTo 1000


-- Problem 27
-- [Quadratic primes](http://projecteuler.net/problem=27)

-- Facts:
-- f(0) = b hence b must be a prime number
-- f(1) = 1 + a + b hence a must be an odd number unless b = 2 is chosen[1].
--    [1] When b = 2, f(2) = 6 + 2a is not a prime number hence disregard the case.

formula027 :: Int -> Int -> Int -> Int
formula027 a b n = n*n + a*n + b

numberOfConsecutivePrimes :: Int -> Int -> Int
numberOfConsecutivePrimes a b = length . takeWhile Utils.isPrime . map f $ [0..]
  where f = formula027 a b

prob027 :: Int
prob027 = p * q
  where 
    xs = [(numberOfConsecutivePrimes a b, a, b) | 
                  a <- [-999,-997..999],
                  b <- Utils.primesTo 999 ]
    (_, p, q) = maximum xs


-- Problem 28
-- [Number spiral diagonals](http://projecteuler.net/problem=28)

-- 1001 x 1001 grid corresponds to k = 500
prob028 :: Int
prob028 = sum $ (1 : (concatMap stage [1..500]))
  where 
    stage :: Int -> [Int]
    stage k = map (\i -> i*2*k + (2*k - 1)^2) [1,2,3,4]


-- Problem 29
-- [Distinct powers](http://projecteuler.net/problem=29)
prob029 :: Int
prob029 = S.size . S.fromList $ [a^b | a <- [2..100], b <- [2..100]]


-- Problem 30
-- [Digit fifth powers](http://projecteuler.net/problem=30)

isDigitsPowerSum :: Int -> Int -> Bool
isDigitsPowerSum r n = n == digitPowerSum
  where digitPowerSum = sum . map (^ r) $ Utils.integerDigits n

-- an upper limit of the search is 354294
-- 5 * 9^5 = 295245
-- 6 * 9^5 = 354294

prob030 :: Int
prob030 = sum $ filter (isDigitsPowerSum 5) [2..354294]


-- IO
select :: Int -> IO Int
select 21 = return prob021
select n
  | n == 22   = prob022
  | otherwise = return $ [prob023, prob024, prob025, prob026, prob027, prob028, prob029, prob030] !! (n - 23)


main :: IO ()
main = getArgs >>= return . read . head >>= select >>= print
