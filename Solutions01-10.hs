{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import Data.Char (digitToInt)
import Data.List (unfoldr)
import qualified Utils as U
import System.Environment (getArgs)

-- | Problem 1
-- [Multiples of 3 and 5](http://projecteuler.net/problem=1)
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, 
-- we get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of
-- all the multiples of 3 or 5 below 1000.
-- 
-- >>> prob001helper 10
-- 23

prob001Helper :: Int -> Int
prob001Helper n = sum . filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) . takeWhile (< n) $ [1..]

prob001 :: Int
prob001 = prob001Helper 1000


-- | Problem 2
-- [Even Fibonacci numbers](http://projecteuler.net/problem=2)
-- Each new term in the Fibonacci sequence is generated by adding the 
-- previous two terms. By starting with 1 and 2, the first 10 terms will be: 
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ... By considering the terms in 
-- the Fibonacci sequence whose values do not exceed four million, 
-- find the sum of the even-valued terms.

prob002 :: Int
prob002 = sum . filter even . takeWhile (< 4000000) $ U.fibonacciSequence



-- | Problem 3
-- [Largest prime factor](http://projecteuler.net/problem=3)
-- The prime factors of 13195 are 5, 7, 13 and 29. 
-- What is the largest prime factor of the number 600851475143?

prob003 :: Int
prob003 = fst . last . U.factorInteger $ 600851475143


-- | Problem 4
-- [Largest palindrome product](http://projecteuler.net/problem=4)
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 x 99. 
-- Find the largest palindrome made from the product of two 3-digit numbers.
prob004 :: Int
prob004 = maximum [ k | i <- [900..999], j <- [i..999], let k = i * j, U.isPalindrome k ]


-- | Problem 5
-- [Smallest multiple](http://projecteuler.net/problem=5)
-- 2520 is the smallest number that can be divided by each of the numbers 
-- from 1 to 10 without any remainder. What is the smallest positive number
-- that is evenly divisible by all of the numbers from 1 to 20?

-- >>> fold1 lcm [1..10]
-- 2520

prob005 :: Int
prob005 = foldr1 lcm [1..20]


-- | Problem 6
-- [Sum square difference](http://projecteuler.net/problem=6)
-- The sum of the squares of the first ten natural numbers is,
-- 1^2 + 2^2 + ... + 10^2 = 385
-- The square of the sum of the first ten natural numbers is,
-- (1+2+ ... +10)^2 = 55^2 = 3025
-- Hence the difference between the sum of the squares of the first 
-- ten natural numbers and the square of the sum is 3025 - 385 = 2640. 
-- Find the difference between the sum of the squares of the first 
-- one hundred natural numbers and the square of the sum.
--
-- >>> diffSumSquared 10
-- 2640

diffSumSquared :: Int -> Int
diffSumSquared n = simpleSum^2 - squaredSum
    where simpleSum  = (n * (n + 1)) `div` 2
          squaredSum = (n * (n + 1) * (2 * n + 1)) `div` 6

prob006 :: Int
prob006 = diffSumSquared 100


-- | Problem 7
-- [10001st prime](http://projecteuler.net/problem=7)
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
-- we can see that the 6th prime is 13. What is the 10001st prime number?
-- 
-- >> U.primes 6
-- [2,3,5,7,11,13]

prob007 :: Int
prob007 = last $ U.primes 10001


-- | Problem 8
-- [Largest product in a series](http://projecteuler.net/problem=8)
-- Find the greatest product of five consecutive digits in the 1000-digit number.

-- | partition n d xs generates sublist of 
-- >>> partition 4 2 [1..7]
-- [[1,2,3,4],[3,4,5,6]]

partition :: Int -> Int -> [a] -> [[a]]
partition n d xs = helper (length xs) xs
  where helper l xs
          | l < n     = []
          | otherwise = take n xs : helper (l-d) (drop d xs)

-- almost identical but which is better?
partition' :: Int -> Int -> [a] -> [[a]]
partition' n d xs = unfoldr takeNOrStop ((length xs), xs)
  where takeNOrStop (l, ys)
          | l < n     = Nothing
          | otherwise = Just (take n ys, (l - d, drop d ys))


prob008 :: Int
prob008 = maximum . map (product . (map digitToInt)) . partition 5 1 $ s
    where s = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"


-- | Problem 9
-- [Special Pythagorean triplet](http://projecteuler.net/problem=9)
-- A Pythagorean triplet is a set of three natural numbers, a, b, c, for which a^2 + b^2 = c^2.
-- For example, 3^2 + 4^2 = 9+16 = 25 = 5^2.
-- There exists exactly one Pythagorean triplet for which a+b+c=1000. Find the product a b c.

troika :: Int -> [[Int]]
troika total = [[a, b, c] | a <- [1..aMax], 
                            let (b, m) = (total * (half - a)) `divMod` (total - a),
                            m == 0,
                            let c = total - a - b,
                            a^2 + b^2 == c^2 ]
  where aMax = total `div` 3 
        half = total `div` 2

prob009 :: Int
prob009 = product . head . troika $ 1000
              

-- | Problem 10
-- [Summation of primes](http://projecteuler.net/problem=10)
-- The sum of the primes below 10 is 2+3+5+7=17. 
-- Find the sum of all the primes below two million.
prob010 :: Int
prob010 = sum $ U.primesTo 2000000


main :: IO ()
main = do
    s:_ <- getArgs
    let n = read s :: Int 
    print $ [prob001, prob002, prob003, prob004, prob005, prob006, prob007, prob008, prob009, prob010] !! (n - 1) 