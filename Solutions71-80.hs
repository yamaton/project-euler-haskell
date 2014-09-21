{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens ((^.))
import qualified Data.List     as List
import qualified Data.Char     as Char
import qualified Data.IntSet   as IntSet
import qualified Data.Set      as Set
import qualified Data.Ratio    as Ratio
import qualified Control.Monad as Monad
import qualified Network.Wreq  as Wreq
import qualified Data.ByteString.Lazy.Char8 as BLC8
import           System.Environment (getArgs)
import qualified Utils

-- | Problem 71
-- [Ordered fractions](http://projecteuler.net/problem=71)
prob071 :: Int
prob071 = undefined




-- | Problem 72
-- [Counting fractions](http://projecteuler.net/problem=72)
prob072 :: Int
prob072 = undefined


-- | Problem 73
-- [Counting fractions in a range](http://projecteuler.net/problem=73)
prob073 :: Int
prob073 = undefined

-- | Problem 74
-- [Digit factorial chains](http://projecteuler.net/problem=74)
prob074 :: Int
prob074 = undefined


-- | Problem 75
-- [Singular integer right triangles](http://projecteuler.net/problem=75)
prob075 :: Int
prob075 = undefined


-- | Problem 76
-- [Counting summations](http://projecteuler.net/problem=76)
prob076 :: Int
prob076 = undefined

-- | Problem 77
-- [Prime summations](http://projecteuler.net/problem=77)
prob077 :: Int
prob077 = undefined


-- | Problem 78
-- [Coin partitions](http://projecteuler.net/problem=78)
prob078 :: IO Int
prob078 = undefined


-- | Problem 79
-- [Passcode derivation](http://projecteuler.net/problem=79)
prob079 :: Int
prob079 = undefined

0
-- | Problem 80
-- [Square root digital expansion](http://projecteuler.net/problem=80)
prob080 :: Int
prob080 = undefined


-- Interface

-- select :: Int -> IO Int
-- select 78 = prob078
-- select n = return $ [prob071, prob072, prob073, prob074, prob075,
--                      prob076, prob077, prob078, prob079, prob080] !! (n - 71)

main :: IO ()
-- main = getArgs >>= return . read . head >>= select >>= print

main = print prob071




