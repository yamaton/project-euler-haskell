{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens               ((^.))
import           Control.Applicative        ((<$>),(<*>))
import qualified Control.Monad              as Monad
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.Char                  as Char
import qualified Data.IntSet                as IntSet
import qualified Data.List                  as List
import qualified Data.Graph                 as Graph
import qualified Data.Number.CReal          as CReal
import qualified Data.Ord                   as Ord
import qualified Data.Ratio                 as Ratio
import qualified Data.Set                   as Set
import qualified Network.Wreq               as Wreq
import           System.Environment         (getArgs)
import qualified Utils

-- | Problem 81
-- [Path sum: two ways](http://projecteuler.net/problem=81)
-- Find the minimal path sum from top left to the bottom right, by moving either down or right.
prob081 :: IO Int
prob081 = do
    dat <- data081
    let rowMax = length dat
    let colMax = length (head dat)
    return $ pathSum081 dat

-- [FIXME] data081 should have better structure
data081 :: IO [[Int]]
data081 = do
    r <- Wreq.get "https://projecteuler.net/project/resources/p081_matrix.txt"
    let raw = r ^. Wreq.responseBody
    return . map (read . (\s -> "[" ++ s ++ "]")) . lines . BLC8.unpack $ raw


pathSum081 :: [[Int]] -> Int
pathSum081 mat = go (rowMax-1) (colMax-1)
  where
    rowMax = length mat
    colMax = length (head mat)
    inf = 99999999999
    go i j
      | i == 0 && j == 0     = (mat !! 0) !! 0
      | i < 0 || rowMax <= i = inf
      | j < 0 || colMax <= j = inf
      | otherwise            = ((mat !! i) !! j) + min (go (i-1) j) (go i (j-1))


-- | Problem 82
-- [Path sum: three ways](http://projecteuler.net/problem=82)
-- Find the minimal path sum from the left column to the right column.
prob082 :: IO Int
prob082 = undefined

data082 :: IO [[Int]]
data082 = do
    r <- Wreq.get "https://projecteuler.net/project/resources/p082_matrix.txt"
    let raw = r ^. Wreq.responseBody
    return . map (read . (\s -> "[" ++ s ++ "]")) . lines . BLC8.unpack $ raw


-- | Problem 83
-- [Path sum: four ways](http://projecteuler.net/problem=83)
-- Find the minimal path sum from the top left to the bottom right
-- by moving left, right, up, and down.
prob083 :: IO Int
prob083 = undefined

data083 :: IO [[Int]]
data083 = do
    r <- Wreq.get "https://projecteuler.net/project/resources/p083_matrix.txt"
    let raw = r ^. Wreq.responseBody
    return . map (read . (\s -> "[" ++ s ++ "]")) . lines . BLC8.unpack $ raw


-- | Problem 84
-- [Monopoly odds](http://projecteuler.net/problem=84)
prob084 :: Int
prob084 = undefined



-- | Problem 85
-- [Counting rectangles](http://projecteuler.net/problem=85)
prob085 :: Int
prob085 = undefined


-- | Problem 86
-- [](http://projecteuler.net/problem=86)
prob086 :: Int
prob086 = undefined

-- | Problem 87
-- [](http://projecteuler.net/problem=87)
prob087 :: Int
prob087 = numSumPowersLessThan 50000000

numSumPowersLessThan :: Int -> Int
numSumPowersLessThan n = IntSet.size $ IntSet.fromList xs
    where xs = [out | i <- ps
                    , j <- qs
                    , k <- rs
                    , let out = i^2 + j^3 + k^4
                    , out < n]
          [ps,qs,rs] = map (\i -> Utils.primesTo . floor $ fromIntegral n ** (1/i)) [2,3,4]


-- | Problem 88
-- [](http://projecteuler.net/problem=88)
prob088 :: Int
prob088 = undefined


-- | Problem 89
-- [](http://projecteuler.net/problem=89)
prob089 :: Int
prob089 = undefined

-- | Problem 90
-- [](http://projecteuler.net/problem=90)
prob090 :: Int
prob090 = undefined


-- Interface

select :: Int -> IO Int
select 81 = prob081
select 82 = prob082
select 83 = prob083
select n = return $ [      0,       0,       0, prob084, prob085,
                     prob086, prob087, prob088, prob089, prob090] !! (n - 81)

main :: IO ()
-- main = getArgs >>= return . read . head >>= select >>= print
main = print prob087
