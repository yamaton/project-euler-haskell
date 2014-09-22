{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}

import           Control.Lens               ((^.))
import qualified Control.Monad              as Monad
import qualified Data.Char                  as Char
import qualified Data.IntSet                as IntSet
import           Data.List                  ((\\))
import qualified Data.List                  as List
import qualified Network.Wreq               as Wreq
-- import qualified Data.Text.Lazy          as TextLazy
-- import qualified Data.Text.Lazy.Encoding as Encoding
-- import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import           System.Environment         (getArgs)
import qualified Utils

-- | Problem 41
-- [Pandigital prime](http://projecteuler.net/problem=41)
-- We shall say that an n-digit number is pandigital if it makes
-- use of all the digits 1 to n exactly once. For example,
-- 2143 is a 4-digit pandigital and is also prime.
-- What is the largest n-digit pandigital prime that exists?

prob041 :: Int
prob041 = maximum pandigitalPrimes
  where
    pandigitalPrimes = [x | n <- [4..9],
                            x <- map Utils.fromDigits . List.permutations $ [1..n],
                            Utils.isPrime x]



-- | Problem 42
-- [Coded triangle numbers](http://projecteuler.net/problem=42)

prob042 :: IO Int
prob042 = Monad.liftM (length . filter isTriangleNumber . map wordScore) data042

data042 :: IO [String]
data042 = do
    r <- Wreq.get "https://projecteuler.net/project/resources/p042_words.txt"
    return $ format042 $ r ^. Wreq.responseBody

-- format042 :: BL.ByteString -> [TextLazy.Text]
-- format042 = TextLazy.splitOn (TextLazy.pack ",")
--           . TextLazy.filter (/= '\"')
--           . Encoding.decodeLatin1

format042 :: BLC8.ByteString -> [String]
format042 = read . BLC8.unpack . flip BLC8.snoc ']' . BLC8.cons '['

isTriangleNumber :: Int -> Bool
isTriangleNumber n = IntSet.member n tns
  where
    -- Assume words have numbers less than (100*101/2)
    tns = IntSet.fromList . map (\n -> n * (n+1) `div` 2) $ [1..100]

wordScore :: String -> Int
wordScore = sum . map (\c -> Char.ord c - base)
  where
    -- Assume all words are uppercase
    base = Char.ord 'A' - 1



-- | Problem 43
-- [Sub-string divisibility](http://projecteuler.net/problem=43)

prob043 :: Int
prob043 = sum . map Utils.fromDigits . filter isPandigital $ out
  where
    (xs:xss) = reverse . map threeDigitMultiples $ [2,3,5,7,11,13,17]
    out = map prependComplement $ connectList xss xs


-- | Pad 0 to the left such that the list has n elements
-- >>> padLeft 0 5 [1,2,3]
-- [0,0,1,2,3]
-- >>> padLeft '.' 8 "abc"
-- ".....abc"
padLeft :: a -> Int -> [a] -> [a]
padLeft e n xs
  | len == n  = xs
  | otherwise = replicate (n - len) e ++ xs
  where len = length xs

-- return three-digit multiples of n, in digits form
threeDigitMultiples :: Int -> [[Int]]
threeDigitMultiples n = map (padLeft 0 3 . Utils.integerDigits) [n, 2*n..999]


{- |
>>> connectList [["bcd", "dfa"], ["zdd", "abc"]] ["zds", "dar", "add"]
[]

>>> connectList [["bcd", "dfa", "eda"], ["zdd", "abc", "zed"]] ["zds", "dar", "cde"]
["abcde","zedar"]

-}
connectList :: (Eq a) => [[[a]]] -> [[a]] -> [[a]]
connectList      _           []  = []
connectList      []        paths = paths
connectList (candids:rest) paths = connectList rest newPaths
  where
    newPaths = [ z:p | (z:zs) <- candids, p <- paths, zs == take 2 p ]


-- | Prepend the largest complement digit to an int digit list
-- >>> prependComplement [1,3,2,4,6,5,7,7,0]
-- [9,1,3,2,4,6,5,7,7,0]

prependComplement :: [Int] -> [Int]
prependComplement xs = p:xs
  where (p:_) = [9,8..1] \\ xs


isPandigital :: [Int] -> Bool
isPandigital xs = (length xs == 10) && (oneToNine == IntSet.fromList xs)
  where oneToNine = IntSet.fromList [0..9]



-- | Problem 44
-- [Pentagon numbers](http://projecteuler.net/problem=44)

prob044 :: Int
prob044 = head [ pm |  m <- [1..2000],
                       j <- [1..1200],
                       let pm = pentagonal m,
                       let pj = pentagonal j,
                       isPentagonal (pm + pj),
                       isPentagonal (pm + 2 * pj)]

pentagonal :: Int -> Int
pentagonal n = n * (3*n -1) `div` 2

-- |
-- >>> isPentagonal 5482660
-- True
isPentagonal :: Int -> Bool
isPentagonal n = (n == d)
  where
    estimate = floor $ (1 + sqrt (1 + 24 * fromIntegral n)) / 6
    xs = drop (estimate - 1) $ map pentagonal [1..]
    d  = head $ dropWhile (< n) xs



-- | Problem 45
-- [Triangular, pentagonal, and hexagonal](http://projecteuler.net/problem=45)
-- Find the lowest triangular number x > 40755 that is also pentagonal and hexagonal.
prob045 :: Int
prob045 = head $ IntSet.toList $ foldr1 IntSet.intersection [ts,ps,hs]
  where
    nMax = 100000    -- [FIXME] This choice of parameter requires try and error!
    ts = toIntSet $ scanl1 (+) [1..]
    ps = toIntSet $ scanl1 (+) [1,4..]
    hs = toIntSet $ scanl1 (+) [1,5..]
    toIntSet = IntSet.fromList . takeWhile (< nMax) . dropWhile (<= 40755)



-- | Problem 46
-- [Goldbach's other conjecture](http://projecteuler.net/problem=46)
-- Find the smallest odd composite that cannot be written as the sum of a prime and twice a square.
prob046 :: Int
prob046 = head $ dropWhile isGoldBach oddComposites
  where
    oddComposites = filter (not . Utils.isPrime) [3,5..]

isGoldBach :: Int -> Bool
isGoldBach n = any isSquare $ map (\p -> (n - p) `div` 2) ps
  where
    -- ps is primes sequence from 3 to (n-2)
    ps = drop 1 $ Utils.primesTo (n-2)

isSquare :: Int -> Bool
isSquare n = x * x == n
  where nn = fromIntegral n
        x = floor $ sqrt nn



-- | Problem 47
-- [Distinct primes factors](http://projecteuler.net/problem=47)
-- Find the first four consecutive integers to have four distinct
-- primes factors. What is the first of these numbers?
prob047 :: Int
prob047 = conseqNDPS 4

conseqNDPS :: Int -> Int
conseqNDPS n = head . head . dropWhile (\xs -> replicate n n /= map ndps xs)
                    $ Utils.partition n 1 [1..]

-- | ndps (Number of Distinct Prime Factors)
-- >>> ndps 12
-- 2
-- >>> ndps 210
-- 4
ndps :: Int -> Int
ndps = length . Utils.factorInteger



-- | Problem 48
-- [Self powers](http://projecteuler.net/problem=48)
-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

prob048 :: Int
prob048 = fromIntegral . (`mod` (10^10)) . sum . map (\i -> i^i) $ xs
  where
    xs = [1..1000] :: [Integer]



-- | Problem 49
-- [Prime permutations](http://projecteuler.net/problem=49)

prob049 :: Int
prob049 = Utils.fromDigits . concatMap Utils.integerDigits $ last troikas
    where
      fourDigitPrimes = filter (> 1000) $ Utils.primesTo 9999
      troikas = [ troika | x   <- fourDigitPrimes
                ,          inc <- [2,4..(5000 - x `div` 2)]
                ,          let troika = [x, x+inc, x+2*inc]
                ,          all Utils.isPrime troika
                ,          allTheSame $ map (List.sort . Utils.integerDigits) troika
                ]

allTheSame :: Eq a => [a] -> Bool
allTheSame xs = and $ zipWith (==) xs (tail xs)



-- | Problem 50
-- [Consecutive prime sum](http://projecteuler.net/problem=50)
-- The longest sum of consecutive primes below one-thousand that adds to a prime,
-- contains 21 terms, and is equal to 953. Which prime, below one-million,
-- can be written as the sum of the most consecutive primes?
prob050 :: Int
prob050 = head [ s | len <- [maxLen, maxLen-1 .. 2]
               ,     i   <- [0..(maxLen - len)]
               ,     let s = sum $ take len . drop i $ xs
               ,     Utils.isPrime s
               ]
  where
    -- The choice of 10000 is little arbitrary ... just a large number
    maxLen = length . takeWhile (< 1000000) . scanl1 (+) $ Utils.primes 10000
    xs = Utils.primes maxLen



-- Interface
select :: Int -> IO Int
select 42 = prob042
select n = return $ [prob041,       0, prob043, prob044, prob045,
                     prob046, prob047, prob048, prob049, prob050] !! (n - 41)

main :: IO ()
main = getArgs >>= return . read . head >>= select >>= print

