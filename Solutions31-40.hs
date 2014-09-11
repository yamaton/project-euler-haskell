{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import qualified Data.List   as List
import qualified Data.Char   as Char
import qualified Data.IntSet as IntSet
import qualified Data.Ratio  as Ratio
import           System.Environment (getArgs)
import           Data.Ratio (Ratio, (%))
import qualified Utils as Utils

-- | Problem 31
-- [Coin sums](http://projecteuler.net/problem=31)
-- How many different ways can £2 be made using any number of coins?
-- Available coins are 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

prob031 :: Int
prob031 = countCoins 200 [1,2,5,10,20,50,100,200]

countCoins :: Int -> [Int] -> Int
countCoins amount coins = f amount sortedCoins
  where
    sortedCoins = (reverse . List.sort) coins
    f :: Int -> [Int] -> Int
    f _ [] = 0
    f 0 _  = 1
    f _ [1] = 1  -- this line may be erased but the pruning makes the computation faster
    f x cs@(z:zs)
      | x < 0     = 0
      | otherwise = f (x - z) cs + f x zs


-- | Problem 32
-- [Pandigital products](http://projecteuler.net/problem=32)
-- Find the sum of all products whose multiplicand/multiplier/product 
-- identity can be written as a 1 through 9 pandigital.
-- 39 × 186 = 7254 is an example.

-- Let d(x) be the numbers of digits of an integer x.
--     10^(d(x)-1) <= x < 10^d(x)
-- Then the product of two integers x, y is
--     10^(d(x) + d(y) -2) <= x * y < 10^(d(x) + d(y)),
-- or
--      d(x) + d(y) - 2 <= d(x * y) < d(x) + d(y).
-- We also know from the pandigital requirement that
--     d(x) + d(y) + d(x * y) = 9.
-- From these relations we have
--     d(x) + d(y) = 5
-- For x <= y, we have (d(x), d(y)) = (1, 4) or (2, 3).

prob032 :: Int
prob032 = sum $ List.nub $ set1 ++ set2
  where
    set1 = [i * j | i <- [1..9],   j <- [1234 .. (9999 `div` i)],        isPandigitalProduct i j]
    set2 = [i * j | i <- [12..99], j <- [123 .. min (9999 `div` i) 999], isPandigitalProduct i j]

isPandigitalProduct :: Int -> Int -> Bool
isPandigitalProduct x y = digits == [1..9]
  where
    digits = List.sort $ concatMap Utils.integerDigits [x, y, x*y]
    

-- | Problem 33
-- [Digit canceling fractions](http://projecteuler.net/problem=33)
-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician 
-- in attempting to simplify it may incorrectly believe that 49/98=4/8, 
-- which is correct, is obtained by cancelling the 9s.
--
-- We shall consider fractions like, 30/50=3/5, to be trivial examples.
--
-- There are exactly four non-trivial examples of this type of fraction,
-- less than one in value, and containing two digits in the numerator and denominator.
--
-- If the product of these four fractions is given in its lowest common terms,
-- find the value of the denominator.

prob033 :: Int
prob033 = Ratio.denominator $ product strangeFractions

strangeFractions :: [Ratio Int]
strangeFractions = [n % d | n <- [10..99], d <- [(n+1)..99], isStrange n d]

isStrange :: Int -> Int -> Bool
isStrange numer denom
  | numer > denom                              = False
  | numer < 10 || 99 < numer                   = False
  | denom < 10 || 99 < denom                   = False
  | numer `mod` 10 == 0 && denom `mod` 10 == 0 = False
  | null canceling                             = False
  | otherwise = y > 0 && (x % y) == (numer % denom)
  where
    [numerDigits, denomDigits] = map Utils.integerDigits [numer, denom]
    canceling = numerDigits `List.intersect` denomDigits
    -- For example canceling is [4, 5] if (numer, denom) = (45, 54)
    -- -> always cancel one element anyway
    c = head canceling  
    [x] = List.delete c numerDigits
    [y] = List.delete c denomDigits



-- | Problem 34
-- [Digit factorials](http://projecteuler.net/problem=34)
-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

-- An upper limit is 9 x 9! = 

prob034 :: Int
prob034 = sum [i | i <- [3..3000000], isCurious i]

factorial :: Int -> Int
factorial n = product [1..n]

isCurious :: Int -> Bool
isCurious n = n == factSum
  where factSum = sum . map factorial $ Utils.integerDigits n



-- | Problem 35
-- [Circular primes](http://projecteuler.net/problem=35)
-- The number, 197, is called a circular prime because all rotations of the digits:
-- 197, 971, and 719, are themselves prime. 
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
-- How many circular primes are there below one million?

prob035 :: Int
prob035 = length . filter isCircularPrime . Utils.primesTo $ 1000000

isCircularPrime :: Int -> Bool
isCircularPrime n = all Utils.isPrime $ map Utils.fromDigits $ digitRotations
  where
    digitRotations = rotations $ Utils.integerDigits n 

rotations :: [a] -> [[a]]
rotations xs = map (\i -> take n . drop i $ zs) [0..(n-1)]
  where 
    n = length xs
    zs = cycle xs




-- | Problem 36
-- [Double-base palindromes](http://projecteuler.net/problem=36)
-- Find the sum of all numbers, less than one million, 
-- which are palindromic in base 10 and base 2.

prob036 :: Int
prob036 = sum . filter isDoublePalindrome $ [1..1000000]
  
isDoublePalindrome :: Int -> Bool
isDoublePalindrome n = isPalindrome (show n) && isPalindrome (Utils.intToBin n)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)






-- | Problem 37
-- [Truncatable primes](http://projecteuler.net/problem=37)
-- The number 3797 has an interesting property. Being prime itself, 
-- it is possible to continuously remove digits from left to right,
-- and remain prime at each stage: 3797, 797, 97, and 7. Similarly
-- we can work from right to left: 3797, 379, 37, and 3.
-- Find the sum of the only eleven primes that are both truncatable
-- from left to right and right to left.
--
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

prob037 :: Int
prob037 = sum . filter isTruncatablePrime . drop 4 . Utils.primesTo $ 1000000

isTruncatablePrime :: Int -> Bool
isTruncatablePrime = all Utils.isPrime . truncates

truncates :: Int -> [Int]
truncates = map Utils.fromDigits . tailsAndInitsWithoutBlank . Utils.integerDigits
  where tailsAndInitsWithoutBlank xs = (init $ List.tails xs) ++ (tail $ List.inits xs)




-- | Problem 38
-- [Pandigital multiples](http://projecteuler.net/problem=38)
-- Take the number 192 and multiply it by each of 1, 2, and 3:
-- 
-- 192 × 1 = 192
-- 192 × 2 = 384
-- 192 × 3 = 576
-- 
-- By concatenating each product we get the 1 to 9 pandigital, 192384576.
-- We will call 192384576 the concatenated product of 192 and (1,2,3)
--
-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5,
-- giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
--
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
-- concatenated product of an integer with (1,2, ... , n) where n > 1?


-- Consider concatenated product of x and (1, 2, .., n).
-- For   n = 2,  5000 <= x <= 9999 if x exists
--       n = 3,   100 <= x <= 333  if x exists
--       n = 4,    10 <= x <= 33   if x exists
-- 5 <= n <= 9,     1 <= x <= 9    if x exists

prob038 :: Int
prob038 = maximum $ concat [lis2, lis3, lis4, lis5]
  where
    lis2 = [z | x <- [5000..9999], let z = concatenatedProduct x [1..2], isPandigital z]
    lis3 = [z | x <- [100..333],   let z = concatenatedProduct x [1..3], isPandigital z]
    lis4 = [z | x <- [10..33],     let z = concatenatedProduct x [1..4], isPandigital z]
    lis5 = [z | x <- [1..9], n <- [5..9], let z = concatenatedProduct x [1..n], isPandigital z]

concatenatedProduct :: Int -> [Int] -> Int
concatenatedProduct x = read . concatMap show . map (*x)

isPandigital :: Int -> Bool
isPandigital n = length xs == 9 && oneToNine == IntSet.fromList xs
  where 
    oneToNine = IntSet.fromList [1..9]
    xs = Utils.integerDigits n




-- | Problem 39
-- [Integer right triangles](http://projecteuler.net/problem=39)
-- If p is the perimeter of a right angle triangle with integral length sides, 
-- {a,b,c}, there are exactly three solutions for p = 120.
-- 
-- {20,48,52}, {24,45,51}, {30,40,50}
-- 
-- For which value of p ≤ 1000, is the number of solutions maximised?

prob039 :: Int
prob039 = Utils.mostFrequent perims
  where 
    perims = [r | p <- [2..floor (sqrt 500)], 
                  q <- [1..(p-1)],
                  odd (p - q),
                  gcd (p * q) (p + q) == 1,
                  r <- [2*p*(p+q), 4*p*(p+q)..1000]
             ]



-- | Problem 40
-- [Champernowne's constant](http://projecteuler.net/problem=40)

prob040 :: Int
prob040 = product . map (d . (10^)) $ [0..6]
  where
    digits = "0123456789"
    xs = digits ++ concatMap (\n -> prependToAll (show n) digits) [1..]
    d n = Char.digitToInt $ xs !! n

prependToAll :: [a] -> [a] -> [a]
prependToAll xs zs = xs ++ (List.intercalate xs . List.transpose $ [zs])



--- Interface

select :: Int -> IO Int
select n = return $ [prob031, prob032, prob033, prob034, prob035, 
                     prob036, prob037, prob038, prob039, prob040] !! (n - 31)


main :: IO ()
main = getArgs >>= return . read . head >>= select >>= print


