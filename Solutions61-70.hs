{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens ((^.))
import           Data.List ((\\))
import qualified Data.List   as List
import qualified Data.Char   as Char
import qualified Data.IntSet as IntSet
import qualified Control.Monad as Monad
import qualified Network.Wreq as Wreq
import qualified Data.Text.Lazy          as TextLazy
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Data.ByteString.Lazy    as BL
import           System.Environment (getArgs)
import qualified Utils

-- | Problem 61
-- [Cyclical figurate numbers](http://projecteuler.net/problem=61)

prob061 :: Int
prob061 = sum . head . filter (not. null) $ concatMap search061 xsss
  where xsss = List.permutations [s3, s4, s5, s6, s7, s8]

-- Return list of cycling, linkabale ints
search061 :: [[Int]] -> [[Int]]
search061 xxs = filter isCyclic $ foldr step seeds (init xxs) 
  where
    seeds = map (:[]) $ last xxs
    step :: [Int] -> [[Int]] -> [[Int]]
    step ns xss = [ n:xs | n <- ns, xs <- xss, isLinkable n (head xs)]

take4dig = takeWhile (< 10000). dropWhile (< 1000)
s3 = take4dig $ scanl1 (+) [1,2..]
s4 = take4dig $ scanl1 (+) [1,3..]
s5 = take4dig $ scanl1 (+) [1,4..]
s6 = take4dig $ scanl1 (+) [1,5..]
s7 = take4dig $ scanl1 (+) [1,6..]
s8 = take4dig $ scanl1 (+) [1,7..]

isLinkable :: Int -> Int -> Bool
isLinkable x y = drop 2 digX == take 2 digY
  where [digX, digY] = map Utils.integerDigits [x, y]

isCyclic :: [Int] -> Bool
isCyclic xs = isLinkable (last xs) (head xs)


-- | Problem 62
-- [](http://projecteuler.net/problem=62)
prob062 :: Int
prob062 = undefined


-- | Problem 63
-- [](http://projecteuler.net/problem=63)
prob063 :: Int
prob063 = undefined

-- | Problem 64
-- [](http://projecteuler.net/problem=64)
prob064 :: Int
prob064 = undefined


-- | Problem 65
-- [](http://projecteuler.net/problem=65)
prob065 :: Int
prob065 = undefined


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

main = print prob061




