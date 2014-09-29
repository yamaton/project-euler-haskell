{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens               ((^.))
import qualified Control.Monad              as Monad
import qualified Data.Bits                  as Bits
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.Char                  as Char
import qualified Data.IntSet                as IntSet
import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe
import           Data.Ratio                 ((%))
import qualified Data.Ratio                 as Ratio
import qualified Network.Wreq               as Wreq
import           System.Environment         (getArgs)
import           Text.Regex.Posix           ((=~))
import qualified Utils


-- | Problem 51
-- [Prime digit replaceAllments](http://projecteuler.net/problem=51)
prob051 :: Int
prob051 = head . wildcardToNums $ searchPrimeDigitRep 7

type Wildcard = String   -- for example "6**71*"

searchPrimeDigitRep :: Int -> Wildcard
searchPrimeDigitRep n = head [w | digits <- [1..]
                             ,    stars <- [1..digits-1]
                             ,    w <- genWildcards digits stars
                             ,    primesInWildcard w == n
                             ]

replaceAll :: Eq a => [a] -> a -> a -> [a]
replaceAll xs from to = map helper xs
  where helper x = if x == from then to else x

primesInWildcard :: Wildcard -> Int
primesInWildcard = length . filter Utils.isPrime . wildcardToNums

-- Profiling says `read` takes over 80% of cpu time in problem 51.
-- Does it mean it consumes unevaluated thunk at this point?
wildcardToNums :: Wildcard -> [Int]
wildcardToNums w = trans ['0' .. '9']
  where
    trans = map read . filter isProperStr . map (replaceAll w '*')

isProperStr :: String -> Bool
isProperStr ('0':_) = False
isProperStr s       = last s `elem` "*13579"

-- | returns wild cards from number of digits and number of stars
genWildcards :: Int -> Int -> [Wildcard]
genWildcards digits stars = filter isProperStr . concatMap Utils.permutations $ xs
  where
    xs = map (++ replicate stars '*') $ Monad.replicateM (digits - stars) "0123456789"



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

-- This is my Haskell-ported version of Peter Norvig's python code.
type Hand = [String] -- example: ["6D", "7H", "AH", "7S", "QC"]
type Rank = Int

prob054 :: IO Int
prob054 = do
    dat <- data054
    return $ length [1 | (h1, h2) <- dat, poker h1 h2 == "left"]

data054 :: IO [(Hand, Hand)]
data054 = do
    r <- Wreq.get "https://projecteuler.net/project/resources/p054_poker.txt"
    let dat = r ^. Wreq.responseBody
    return . map (splitAt 5 . words) . lines . BLC8.unpack $ dat

poker :: Hand -> Hand -> String
poker hand1 hand2
  | (not . null) extra = "none"
  | out == hand1       = "left"
  | otherwise          = "right"
    where out:extra = allMax handRank [hand1, hand2]

allMax :: (Eq a, Ord b) => (a -> b) -> [a] -> [a]
allMax rankFunc hands = filter (\h -> rankFunc h == oneMax) hands
  where oneMax = maximum $ map rankFunc hands

handRank :: Hand -> (Rank, [Rank])
handRank hand
  | isStraight ranks && isFlush hand  = (8, [maximum ranks])
  | isKind 4 ranks                    = (7, [kind 4 ranks, kind 1 ranks])
  | isKind 3 ranks && isKind 2 ranks  = (6, [kind 3 ranks, kind 2 ranks])
  | isFlush hand                      = (5, ranks)
  | isStraight ranks                  = (4, [maximum ranks])
  | isKind 3 ranks                    = (3, kind 3 ranks : ranks)
  | isTwoPair ranks                   = (2, twoPair ranks ++ ranks)
  | isKind 2 ranks                    = (1, kind 2 ranks : ranks)
  | otherwise                         = (0, ranks)
    where ranks = cardRanks hand

-- |
-- >>> cardRanks ["6D", "7H", "AH", "7S", "QC"]
-- [14,12,7,7,6]
cardRanks :: Hand -> [Rank]
cardRanks hand
  | rank == [14,5,4,3,2] = [5,4,3,2,1]
  | otherwise            = rank
    where rank = reverse . List.sort
               $ map (Maybe.fromJust . (`List.elemIndex` "--23456789TJQKA") . head) hand

isFlush :: Hand -> Bool
isFlush hand = length (List.nub suits) == 1
  where suits = map last hand

isStraight :: [Rank] -> Bool
isStraight ranks = maximum ranks - minimum ranks == 4 && length (List.nub ranks) == 5

-- |
-- >>> kind 2 [14,14,14,7,7]
-- 7
-- >>> kind 3 [10,3,3,3,2]
-- 3
-- >>> kind 3 [10,3,3,2,2]
-- 0
kind :: Int -> [Rank] -> Rank
kind n ranks
  | null outcome = 0
  | otherwise    = head outcome
    where outcome = dropWhile (\x -> count x ranks /= n) ranks

-- |
-- >>> twoPair [10,3,3,2,2]
-- [3,2]
-- >>> twoPair [10,3,2,2,2]
-- []
twoPair :: [Rank] -> [Rank]
twoPair ranks
  | 0 /= highPair && highPair /= lowPair  = [highPair, lowPair]
  | otherwise                             = []
    where highPair = kind 2 ranks
          lowPair  = kind 2 (reverse ranks)

isKind :: Int -> [Rank] -> Bool
isKind n ranks = 0 /= kind n ranks

isTwoPair :: [Rank] -> Bool
isTwoPair ranks = not . null $ twoPair ranks

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (== x) xs



-- | Problem 55
-- [Lychrel numbers](http://projecteuler.net/problem=55)
--
prob055 :: Int
prob055 = length $ filter isLychrel [1..9999]

-- | Take sum of its reversed digit  ex) 349 + 943 = 1292
-- >>> digitRevSum 349
-- 1292
-- >>> digitRevSum 4213
-- 7337
digitRevSum :: (Integral a, Show a, Read a) => a -> a
digitRevSum n = n + (read . reverse . show) n

-- | repetitive application of digitRevSum easily exceeds maxBound of Int
--
-- >>> isLychrel 349
-- False
-- >>> isLychrel 196
-- True
isLychrel :: Int -> Bool
isLychrel n = helper (fromIntegral n) 0
  where
    helper :: Integer -> Int -> Bool
    helper _ 50  = True
    helper p count
      | Utils.isPalindrome p = False
      | otherwise            = helper (digitRevSum p) (count+1)



-- | Problem 56
-- [Powerful digit sum](http://projecteuler.net/problem=56)
prob056 :: Int
prob056 = maximum [digitSum (a^b) | a <- [80..100], b <- [80..100]]

digitSum :: Integer -> Int
digitSum = sum . Utils.integerDigits



-- | Problem 57
-- [Square root convergents](http://projecteuler.net/problem=57)
prob057 :: Int
prob057 = length . filter moreDigitsInNumerator . take 1000 $ expansionSeries

moreDigitsInNumerator :: Rational -> Bool
moreDigitsInNumerator r = (a > b)
  where
    [a, b] = map digitLen [Ratio.numerator r, Ratio.denominator r]
    digitLen = length . show

-- |
-- contFraction [a1, a2, a3, a4] gives
-- a1 + 1 / (a2 + 1 / (a3 + 1 / a4))
--
-- >>> contFraction [1,2,2,2]
-- 17 % 12
contFraction :: [Int] -> Rational
contFraction xs = foldr (\x p -> fromIntegral x + 1 / p) r (init xs)
  where r = fromIntegral (last xs) % 1

-- |
-- >>> take 5 $ expansionSeries
-- [3 % 2,7 % 5,17 % 12,41 % 29,99 % 70]
expansionSeries :: [Rational]
-- drop 2 to remove [] and [1] from the source
expansionSeries = map contFraction . drop 2 . List.inits $ (1:repeat 2)



-- | Problem 58
-- [Spiral primes](http://projecteuler.net/problem=58)
prob058 :: Int
prob058 = 2 * stages + 1
  where
    moreThanTenPercent (a, b) = 10 * a > b
    stages = 1 + length (takeWhile moreThanTenPercent primeCount)

-- Count (a) number of diagonals (b) number of primes out of them, for each stage.
-- And return infinite series of (a, b) for increasing stage.
-- Note that the center (number 1) contributes to (0, 1) the initial value to scanl.
primeCount :: [(Int, Int)]
primeCount = tail $ scanl (\(a, b) xs -> (a + countPrimes xs, b + 4)) (0, 1) xss
  where
    xss = map (\i -> [(2*i-1)^2 + 2*i,
                      (2*i-1)^2 + 4*i,
                      (2*i+1)^2 - 2*i,
                      (2*i+1)^2      ] ) [1..]
    countPrimes = length . filter Utils.isPrime



-- | Problem 59
-- [XOR decryption](http://projecteuler.net/problem=59)
prob059 :: IO Int
prob059 = Monad.liftM (sum . map Char.ord . decrypt) data059

decrypt :: [Int] -> String
decrypt dat = head [ message | key <- Monad.replicateM 3 ['a'..'z']
                      ,           let message = applyKey dat key
                      ,           makeSense message
                      ]

makeSense :: String -> Bool
makeSense s = s =~ "[A-Z][a-z,' ]+[\\.!] [A-Z][a-z,' ]+[\\.!]"

data059 :: IO [Int]
data059 = do
  r <- Wreq.get "https://projecteuler.net/project/resources/p059_cipher.txt"
  let dat = r ^. Wreq.responseBody
  return . read . (\s -> "[" ++ s ++ "]") . BLC8.unpack $ dat

applyKey :: [Int] -> String -> String
applyKey raw key = map Char.chr $ zipWith Bits.xor raw (cycle xs)
  where xs = map Char.ord key



-- | Problem 60
-- [Prime pair sets](http://projecteuler.net/problem=60)

-- When two prime numbers a and b give primes when concatenated in both orders,
-- I take it as an edge connecting two nodes (a,b). Mapping the problem into
-- a graph, I look for maximum complete subgraphs (cliques) that have five nodes.


-- | Even slower version
--
-- prob060' :: Int
-- prob060' = sum . head . filter (\vs -> length vs == 5) vss
--   where
--     maxNodes = 2000
--     vss = head . dropWhile (\zss -> maximum (map length zss) < 5)
--                . tail . scanl step [] $ Utils.primes maxNodes
--     step :: [[Int]] -> Int -> [[Int]]
--     step cliques n = [ n:vs| vs <- cliques, all (isConnected' n) vs ] ++ cliques ++ [ [n] ]
--
-- isConnected' :: Int -> Int -> Bool
-- isConnected' n1 n2 = all Utils.isPrime [p, q]
--   where p = concatNums n1 n2
--         q = concatNums n2 n1


type Vertex = Int
type Vertices = IntSet.IntSet
type Edge = (Vertex, Vertex)
type Graph = [Edge]

-- This should be rewritten s.t. the clique is counted as the graph grows
prob060 :: Int
prob060 = minimum [sum (IntSet.toList vs)| vs <- findCliques g, IntSet.size vs == 5]
  where
    maxNodes = 1500  -- [FIXME] This parameter is set by trials and errors
    pairs = map listToTuple $ Utils.combinations 2 (Utils.primes maxNodes)
    g = filter isConnected pairs

listToTuple :: [a] -> (a, a)
listToTuple [x, y] = (x, y)
listToTuple _      = error "bad format"

concatNums :: Int -> Int -> Int
concatNums x y = Utils.fromDigits $ concatMap Utils.integerDigits [x, y]

isConnected :: (Int, Int) -> Bool
isConnected (n1, n2) = all Utils.isPrime [p, q]
  where p = concatNums n1 n2
        q = concatNums n2 n1

vertices :: Graph -> Vertices
vertices g = IntSet.fromList $ n1 ++ n2
  where n1 = map fst g
        n2 = map snd g

findCliques :: Graph -> [Vertices]
findCliques graph = bronKerbosch IntSet.empty (vertices graph) IntSet.empty
  where
    neighborsOf :: Vertex -> Vertices
    neighborsOf u = IntSet.fromList vs
      where vs = [if b == u then a else b | (a, b) <- graph, a == u || b == u]

    pickPivot :: Vertices -> Vertices -> Vertex
    pickPivot s1 s2 = head $ IntSet.elems s1 ++ IntSet.elems s2

    bronKerbosch :: Vertices -> Vertices -> Vertices -> [Vertices]
    bronKerbosch r p x
      | IntSet.null p && IntSet.null x = [r]
      | otherwise                      = concat $ snd mapAccResult
      where
        pivot = pickPivot p x
        subVertexList = IntSet.elems $ p IntSet.\\ (neighborsOf pivot)
        mapAccResult = List.mapAccumL step (p, x) subVertexList
        step (p',x') v' = ((nextP, nextX), nextBron)
          where
            nv = neighborsOf v'
            nextP = IntSet.delete v' p'
            nextX = IntSet.insert v' x'
            nextBron = bronKerbosch (IntSet.insert v' r)
                                    (IntSet.intersection nv p')
                                    (IntSet.intersection nv x')


select :: Int -> IO Int
select 54 = prob054
select 59 = prob059
select n = return $ [prob051, prob052, prob053, 0, prob055,
                     prob056, prob057, prob058, 0, prob060] !! (n - 51)


main :: IO ()
main = getArgs >>= return . read . head >>= select >>= print
-- main = data059 >>= print . decrypt
-- main = print prob060'
