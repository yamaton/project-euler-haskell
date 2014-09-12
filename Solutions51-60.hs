{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens ((^.))
import           Data.List ((\\))
import qualified Data.List   as List
import qualified Data.Char   as Char
import qualified Data.IntSet as IntSet
import qualified Data.Maybe  as Maybe
import qualified Network.Wreq as Wreq
import qualified Data.Text.Lazy          as TextLazy
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import           System.Environment (getArgs)
import qualified Utils as Utils

-- | Problem 51
-- [Prime digit replacements](http://projecteuler.net/problem=51)

prob051 :: Int
prob051 = undefined


-- | Problem 52
-- [Permuted multiples](http://projecteuler.net/problem=52)
prob052 :: Int
prob052 = head $ dropWhile (not . isPermutedMultiples 6) [1..]

isPermutedMultiples :: Int -> Int -> Bool
isPermutedMultiples multiplyer n = allTheSame xs
  where xs = map (List.sort . Utils.integerDigits . (*n)) [1..multiplyer]

allTheSame :: Eq a => [a] -> Bool
allTheSame xs = and $ zipWith (==) xs (tail xs)


-- | Problem 53
-- [Combinatoric selections](http://projecteuler.net/problem=53)
prob053 :: Int
prob053 = length $ filter (> 10^6) xs
  where 
    xs :: [Integer]
    xs = concat $ pascalTriangle 100

pascalMap :: Integral a => [a] -> [a]
pascalMap xs = zipWith (+) (0:xs) (xs ++ [0])

pascalTriangle :: Int -> [[Integer]]
pascalTriangle n = take (n+1) $ iterate pascalMap [1]



-- | Problem 54
-- [Poker hands](http://projecteuler.net/problem=54)
prob054 :: IO Int
prob054 = undefined

data054 :: IO [[String]]
data054 = do
  r <- Wreq.get "https://projecteuler.net/project/resources/p054_poker.txt"
  let dat = r ^. Wreq.responseBody
  return . map words . lines . BLC8.unpack $ dat



-- | Problem 55
-- [Lychrel numbers](http://projecteuler.net/problem=55)
prob055 :: Int
prob055 = length $ filter isLychrel [1..9999]

palindromeMap :: Int -> Int
palindromeMap n = n + (Utils.fromDigits . reverse . Utils.integerDigits) n 

isLychrel :: Int -> Bool
isLychrel n = helper n 0
  where 
    helper :: Int -> Int -> Bool
    helper _ 50              = True
    helper p count
      | Utils.isPalindrome p = False
      | otherwise            = helper (palindromeMap p) (count + 1)  



-- | Problem 56
-- [Powerful digit sum](http://projecteuler.net/problem=56)
prob056 :: Int
prob056 = maximum [digitSum (a^b) | a <- [80..100], b <- [80..100]]
  
digitSum :: Integer -> Int
digitSum = sum . Utils.integerDigits


-- | Problem 57
-- [Square root convergents](http://projecteuler.net/problem=57)
prob057 :: Int
prob057 = undefined


-- | Problem 58
-- [Spiral primes](http://projecteuler.net/problem=58)
prob058 :: Int
prob058 = undefined


primeRatio :: Int -> Float
primeRatio layers = nPrimes / (4 * fromIntegral layers + 1)
  where
    xs = concatMap (\i -> [(2*i-1)^2 + 2*i, 
                           (2*i-1)^2 + 4*i,
                           (2*i+1)^2 - 2*i,
                           (2*i+1)^2]      ) [1..layers]
    nPrimes = fromIntegral . length $ filter Utils.isPrime xs



-- | Problem 59
-- [XOR decryption](http://projecteuler.net/problem=59)
prob059 :: IO Int
prob059 = undefined

data059 :: IO [Int]
data059 = do
  r <- Wreq.get "https://projecteuler.net/project/resources/p059_cipher.txt"
  let dat = r ^. Wreq.responseBody
  return . map (read . BLC8.unpack) . BLC8.split ',' $ dat



-- | Problem 60
-- [Prime pair sets](http://projecteuler.net/problem=60)
prob060 :: Int
prob060 = undefined



-- Interface

-- select :: Int -> IO Int
-- select 54 = prob054
-- select 59 = prob059
-- select n = return $ [prob051, prob052, prob053, 0, prob055,
--                      prob056, prob057, prob058, 0, prob060] !! (n - 51)

main :: IO ()
-- main = getArgs >>= return . read . head >>= select >>= print
main = print $ Utils.fromDigits [0,7,9]



