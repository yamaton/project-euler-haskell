{-# LANGUAGE TemplateHaskell #-}

import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH                    (defaultMainGenerator)
import           Test.HUnit                           ((@=?), (@?), (@?=))
import           Test.QuickCheck                      ((==>))

import qualified Data.List                            as List
import qualified Data.Set                             as Set
import qualified Data.IntSet                          as IntSet
import           Utils


main :: IO ()
main = $(defaultMainGenerator)


---------------------------------------------
--     Test  partition, partition', chunksOf
---------------------------------------------
prop_partition1 n d xs =
    (n > 0 && d > 0) ==>
    partition n d xs == partition' n d xs
        where
          types = (n :: Int, d :: Int, xs :: [Int])

prop_partition2 n xs =
    (n > 0) ==>
    partition n n xs == chunksOf n xs
        where
          types = (n :: Int, xs :: [Int])


case_partition1 =
   partition' 4 2 [1..7] @?= [[1,2,3,4],[3,4,5,6]]

case_partition2 =
   partition' 2 2 [1..7] @?= [[1,2],[3,4],[5,6]]

case_partition3 =
   partition' 2 3 [1..7] @?= [[1,2],[4,5]]



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

case_splitOn1 =
    splitOn ',' "aa,bc,cd,e" @?= ["aa","bc","cd","e"]


-----------------------------------
--     Test  permutations
-----------------------------------





-----------------------------------
--     Test  combinations
-----------------------------------
-- prop_combi n xs =
--     0 < n && n < 3 && n <= length xs ==>
--     lhs xs == rhs xs
--       where
--         types = (n :: Int, xs :: [Int])
--         lhs :: [Int] -> Set.Set IntSet.IntSet
--         lhs = Set.fromList . map IntSet.fromList
--                            . filter (\xs -> length xs == n)
--                            . List.subsequences
--         rhs :: [Int] -> Set.Set IntSet.IntSet
--         rhs = Set.fromList . map IntSet.fromList . combinations n

case_combi1 =
    combinations 2 [1..4] @?= [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]


--------------------------------------------
--     Test  combinations with replacement
--------------------------------------------

case_combiWithRep =
    combinationsWithReplacement 2 "abc" @?= ["aa","ab","ac","bb","bc","cc"]
