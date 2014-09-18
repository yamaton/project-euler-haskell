{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens ((^.))
import           Data.List ((\\))
import qualified Data.List   as List
import qualified Data.Char   as Char
import qualified Data.Set as Set
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

main = print $ findPermsFromCubes 3 8
