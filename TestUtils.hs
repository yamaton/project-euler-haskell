{-# LANGUAGE TemplateHaskell #-}

import Test.Framework.TH (defaultMainGenerator)
import Test.HUnit ((@?), (@?=), (@=?))
import Test.QuickCheck ((==>))
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Utils


-----------------------------------
--     Test  roundRobin
-----------------------------------
prop_roundRobin xss = 
    lhs xss == rhs xss
        where 
            types = xss :: [[Int]]
            lhs = length . concat
            rhs = length . roundRobin

case_roundRobin1 = 
    roundRobin ["abc", "d", "ef"] @?= "adebfc"
    
case_roundRobin2 = 
    roundRobin ["abc"] @?= "abc"

case_roundRobin3 = 
    roundRobin ["abc", "d", ""] @?= "adbc"


-----------------------------------
--     Test  splitOn
-----------------------------------

---- | This is wrong: consider x = 0,  xs = [0].  Or x = 0 and xs = [0,2]
--prop_splitOn x xs = 
--    xs == List.intercalate [x] (splitOn x xs)
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
--            lhs :: Ord a => [a] -> Set.Set (Set.Set a)
--            lhs = Set.fromList . map Set.fromList . filter ((== n) . length) . List.subsequences
--            rhs :: Ord a => [a] -> Set.Set (Set.Set a)
--            rhs = Set.fromList . map Set.fromList . combinations n
          
case_combi1 = 
    combinations 2 [1..4] @?= [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]


--------------------------------------------
--     Test  combinations with replacement 
--------------------------------------------

case_combiWithRep = 
    combinationsWithReplacement 2 "abc" @?= ["aa","ab","ac","bb","bc","cc"]



main :: IO ()
main = $(defaultMainGenerator)
