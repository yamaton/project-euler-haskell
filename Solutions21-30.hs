{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import Control.Lens ((^.))
import Data.Char (ord)
import Data.Text.Encoding as TE
import qualified Data.List as L
--import qualified Data.Set as S
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

--import System.Environment (getArgs)
import qualified Utils as U



-- | Problem 21
-- [Amicable Numbers](http://projecteuler.net/problem=21)
-- Evaluate the sum of all the amicable numbers under 10000.

isAmicable :: Int -> Bool
isAmicable 1 = False
isAmicable n = (n /= m) && (n == divisorSum m)
  where divisorSum :: Int -> Int
        divisorSum x = sum (U.divisors x) - x
        m = divisorSum n

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
format022 = L.sort . U.splitOn ',' . filter (/= '\"') . T.unpack . TE.decodeLatin1 . B.concat . BL.toChunks

name2int :: String -> Int
name2int = sum . map alph2int
  where alph2int c = ord c - ord 'A' + 1

totalScore :: [String] -> Int
totalScore = sum . zipWith (\i s -> i * name2int s) [1..]


-- Problem 23
-- [Names scores](http://projecteuler.net/problem=23)
-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

isAbundant :: Int -> Bool
isAbundant n = sumDivisors n > 2 * n
    where sumDivisors = sum . U.divisors

abundantList :: [Int]
abundantList = filter isAbundant [1..28123]

isNotSumOfTwoAbundants :: Int -> Bool
isNotSumOfTwoAbundants n = null $ L.intersect xs abundantList
    where xs = map (n -) abundantList

prob023 :: Int
prob023 = sum $ filter isNotSumOfTwoAbundants [1..28123]



-- Problem 24
-- [Lexicographic permutations](http://projecteuler.net/problem=24)
--
prob024 :: Int
prob024 = U.fromDigits (x !! 1000000)
    where x = L.sort $ L.permutations [0..9]

main :: IO ()
main = print prob024
--main = prob022 >>= print