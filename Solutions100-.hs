{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens               ((^.))
import qualified Control.Monad              as Monad
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.Char                  as Char
import qualified Data.IntSet                as IntSet
import qualified Data.List                  as List
import qualified Network.Wreq               as Wreq
import           System.Environment         (getArgs)
import qualified Utils


-- Problem 127
--
prob127 :: Int
prob127 = sum [c | c <- [3..9999]
              ,    2 * radical c < c        -- truncate unnecessary computations
              ,    a <- [1..lessThanHalf c] -- to ensure a < b = c - a
              ,    let b = c - a            -- b such that a + b = c
              ,    foldr1 gcd [a,b,c] == 1  -- a b c are coprime.
              ,    prodRadical a b c < c    -- rad(a b c) < c
              ]
  where
    lessThanHalf x
      | even x    = xHalf - 1
      | otherwise = xHalf
      where xHalf = x `div` 2

radical :: Int -> Int
radical = product . map fst . Utils.factorInteger

-- | rad (a b c) = rad(a) rad(b) rad(c) when (a,b,c) are coprime
prodRadical :: Int -> Int -> Int -> Int
prodRadical a b c = product $ map radical [a, b, c]




-- Problem 129
-- [Repunit divisibility](https://projecteuler.net/problem=129)
prob129 :: Int
prob129 = head $ filter (\n -> funcA n > 1000000) xs
  where
    -- Starting the number from 10^6 is a guess from the pattern of
    -- n in A(n) > 10^3 and A(n) > 10^4.
    xs = filter (\n -> gcd 10 n == 1) [1000000..]

-- | A(n) be least number of k such that prepunit R(k) is divisible by n
-- >>> funcA 7
-- 6
-- >>> funcA 41
-- 5
funcA :: Int -> Int
funcA n = go 1 (rem 10 p)
  where
    p = 9*n
    go k 1     = k
    go k state = go (k+1) (rem (10*state) p)


-- Problem 135
--
prob135 :: Int
prob135 = length $ filter isGood135 [1..1000000]

isGood135 :: Int -> Bool
isGood135 n
  | Utils.divisorsCount n < 12 = False
  | otherwise                  = length pairs == 10
  where as = Utils.divisors n
        bs = reverse as
        pairs = filter cond $ zip as bs
        cond (a, b) = b < 3*a && mod (a+b) 4 == 0



-- Problem 429
prob429 :: Int
prob429 = productMod terms p
  where
    n = 100000000
    p = 1000000009
    primes = Utils.primesTo n
    multiplicities = map (multOfFact n) primes
    terms = zipWith (\a b -> (powMod a (2*b) p + 1) `mod` p) primes multiplicities

-- | powMod x n p is equivalent to x^n (`mod` p)
-- Exponentiation by square is ued
-- http://en.wikipedia.org/wiki/Exponentiation_by_squaring#Basic_method
powMod :: Int -> Int -> Int -> Int
powMod x 1 _  = x
powMod x n p
  | even n    = powMod x2 nHalf p
  | otherwise = (x * powMod x2 nHalf p) `mod` p
  where x2 = (x*x) `mod` p
        nHalf = n `div` 2

-- | productMod xs p  should be equivalent to  (product xs) (`mod` p)
productMod :: [Int] -> Int -> Int
productMod xs p = foldr1 (\x acc -> (x * acc) `mod` p) xs

-- | multiplicity of p in the factorization of n!
-- Using the Lagrange's method
multOfFact :: Int -> Int -> Int
multOfFact n p = sum . takeWhile (> 0) $ map (\i -> n `div` p^i) [1..]


main :: IO ()
-- main = getArgs >>= return . read . head >>= select >>= print
main = print prob127
