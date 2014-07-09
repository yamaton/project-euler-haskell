{-# LANGUAGE TemplateHaskell #-}

import Test.Framework.TH (defaultMainGenerator)
import Test.HUnit ((@?), (@?=), (@=?))
import Test.QuickCheck ((==>))
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Set as S
import Data.List as L
import Utils


-----------------------------------
--     Test  roundRobin
-----------------------------------
prop_roundRobin xss = 
    lhs xss == rhs xss
        where 
            types = (xss :: [[Int]])
            lhs = length . foldr (++) []
            rhs = length . roundRobin

case_roundRobin1 = 
    roundRobin ["abc", "d", "ef"] @?= "adebfc"


-----------------------------------
--     Test  splitOn
-----------------------------------

---- | This is wrong: consider x = 0,  xs = [0].  Or x = 0 and xs = [0,2]
--prop_splitOn x xs = 
--    xs == L.intercalate [x] (splitOn x xs)
--        where types = (x :: Char, xs :: String)

case_splitOn1 = 
    splitOn ',' "aa,bc,cd,e" @?= ["aa","bc","cd","e"]



-----------------------------------
--     Test  combinations
-----------------------------------
--prop_combi n xs 
--    = 0 < n && n < 3 && n <= length xs ==> lhs xs == rhs xs
--        where 
--            types = (n :: Int, xs :: [Int])
--            lhs :: Ord a => [a] -> S.Set (S.Set a)
--            lhs = S.fromList . map S.fromList . filter ((== n) . length) . L.subsequences
--            rhs :: Ord a => [a] -> S.Set (S.Set a)
--            rhs = S.fromList . map S.fromList . combinations n
          
case_combi1 = 
    combinations 2 [1..4] @?= [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]


--------------------------------------------
--     Test  combinations with replacement 
--------------------------------------------

case_combiWithRep = 
    combinationsWithReplacement 2 "abc" @?= ["aa","ab","ac","bb","bc","cc"]





main :: IO ()
main = $(defaultMainGenerator)