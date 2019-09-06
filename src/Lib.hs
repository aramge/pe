{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( someFunc
  ) where

import qualified Data.Sort
import qualified Math.NumberTheory.Primes as Pr
import qualified Math.NumberTheory.Primes.Factorisation as F
import qualified Math.NumberTheory.Primes.Testing as P

import qualified Data.Function (on)
import qualified Data.List as L
import qualified Data.List.Extra as X
import qualified Data.Ord

--import qualified Math.NumberTheory.Primes.Factorisation
import qualified System.IO

import qualified Data.Char
import qualified Data.Text as T

--import qualified Data.Set as Set
import qualified Numeric

import Control.Arrow ((&&&))
import qualified Data.Array as A
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--import qualified Data.MemoCombinators (memo2, integral) as Memo
import qualified Data.MemoCombinators as Memo
import Data.Ratio
import qualified Math.Combinat.Partitions.Integer as C

-- import qualified Data.Map as Map
someFunc :: IO ()
--someFunc = putStrLn "someFunc"
someFunc = print sol60

-- ---------------------- project euler 51 -----------------------------
-- [1..]   find smallest prime
-- 34267
-- "34267"           show
-- 5                 length
-- 1 .. 2^5 -1
-- 0001 .. 1111      justify.Right (5-1) . decToBin
-- "_,_,_,1"   replace 0 -> "_"
-- "_,_,_,[0..9]  map [0..9]
-- "342[0..9]7" map stringLast
-- 342[0..9]7 map read::Integer
-- 1       map if P.isPrime 1 else 0
-- 7          map sum
--            (>8) ? finish else recur
sol51 x
  | x `mod` 2 == 0 = sol51 $ x + 1
  | bix /= [] = bix
  | otherwise = sol51 $ x + 2
  where
    bix =
      filter (\l -> 8 <= (sum $ map (oneForTrue . P.isPrime) l)) $
      (map . map) (fromIntegral . read . (stringLast xs)) $
      (\biii -> [[X.replace "1" c b | c <- map show [0 .. 9]] | b <- biii]) $
      map ((justRight (lx - 1)) . (X.replace "0" "_") . decToBin) $
      [1 .. 2 ^ (lx - 1) - 1]
    lx = length xs
    xs = show x

sol51' x
  | bix /= [] = (x, bix)
  | not $ P.isPrime x = sol51' $ x + 2
  | otherwise = sol51' $ x + 2
  where
    bix =
      filter (\l -> 8 <= (sum $ map (oneForTrue . P.isPrime) l)) $
      filter (x `elem`) $
      (map . map) (fromIntegral . read . (stringLast xs)) $
      (\biii -> [[X.replace "1" c b | c <- map show [0 .. 9]] | b <- biii]) $
      map ((justRight (lx - 1)) . (X.replace "0" "_") . decToBin) $
      [1 .. 2 ^ (lx - 1) - 1]
    lx = length xs
    xs = show x

--[[121313,222323,323333,424343,525353,626363,727373,828383,929393]]
--  _2_3_3 ___3_3 _2____ _2_3_3 _2_3_3
--  120303
-- check8Primes times mask numb
decToBin' :: Integral i => i -> [Char]
decToBin' 0 = "0"
decToBin' d
  | d `mod` 2 == 1 = (decToBin' $ d `div` 2) ++ "1"
  | d `mod` 2 == 0 = (decToBin' $ d `div` 2) ++ "0"

decToBin d = tail $ decToBin' d

justRight n ss
  | n > lss = (take (n - lss) $ repeat '_') ++ ss
  | otherwise = ss
  where
    lss = length ss

oneForTrue :: Bool -> Integer
oneForTrue x
  | x = 1
  | otherwise = 0

charLast c d
  | d == '_' = c
  | otherwise = d

stringLast = zipWith' charLast

zipWith' _ [] [] = []
zipWith' f [] (y:ys) = y : zipWith' f [] ys
zipWith' f (x:xs) [] = x : zipWith' f xs []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- ---------------------- project euler 52 -----------------------------
sol52 =
  filter (\(a, b) -> b) $
  map (\(a, b) -> (a, allTheSame b)) $
  map (\(a, b) -> (a, map (X.sort . X.nubOrd . show) b)) $
  [(x, [2 * x, 3 * x, 4 * x, 5 * x, 6 * x]) | x <- [1 ..]]

allTheSame [x, y] = (==) x y
allTheSame (x1:x2:xs) = (==) x1 x2 && allTheSame (x2 : xs)

-- ---------------------- project euler 53 -----------------------------
fact 0 = 1
fact n = foldr1 (*) [1 .. n]

combinations' 1 1 = 1
combinations' n r = fact n `div` fact r `div` fact (n - r)

sol53 =
  length
    [ combinations' n r
    | n <- [1 .. 100]
    , r <- [1 .. n]
    , combinations' n r > 1000000
    ]

-- ---------------------- project euler 55 -----------------------------
isPalindrome a = (==) a $ reverse a

intIsPalindrome i = isPalindrome $ show i

isLychrel n i
  | n == 0 = True
  | intIsPalindrome ly = False
  | otherwise = isLychrel (n - 1) ly
  where
    ly = i + (read $ reverse $ show i)

sol55 = length $ filter (\l -> l) $ map (isLychrel 50) [1 .. 9999] -- ---------------------- project euler 56 -----------------------------

sumOfDigits :: Integer -> Integer
sumOfDigits i =
  foldr (+) 0 $ map ((read :: String -> Integer) . (: [])) $ show i

sol56 =
  last $
  X.sortOn
    (\(_, _, c) -> c)
    [(a, b, sumOfDigits $ a ^ b) | a <- [1 .. 100], b <- [1 .. 100]]

-- ---------------------- project euler 78 -----------------------------
-- 8 0 -> 1x8
-- 7 1 ->     1x7                         1x1
-- 6 2 ->         1x6                 1x2 2x1
-- 5 3 ->             1x5     1x3     1x2 3x1
-- 4 4 ->                 2x4 1x3     2x2 4x1
-- 3 5 ->                     2x3 1x3 1x2 3x1
-- 2 6 ->                             4x2 6x1
-- 1 8 ->                                 8x1
--
-- 9 0 -> 1x9
-- 8 1 ->     1x8                         1x1
-- 7 2 ->         1x7                 1x2 2x1
-- 6 3 ->             1x6         1x3 1x2 3x1
-- 5 4 ->                 1x5 1x4 1x3 2x2 4x1
-- 4 5 ->                     2x4 2x3 2x2 5x1
-- 3 6 ->                         3x3 3x2 6x1
-- 2 7 ->                             4x2 7x1
-- 1 9 ->                                 9x1
-- Selbstgemacht
piles :: Integer -> Integer
piles n = sum [pilesHelper n i | i <- [1 .. n]]

pilesHelper :: Integer -> Integer -> Integer
pilesHelper n k
  | k > n = 0
  | n == k = 1
  | k == 0 = 0
  | otherwise = ((pilesHelper (n - k) k) + (pilesHelper (n - 1) (k - 1)))

-- oeis.org
a000041 n = a000041_list !! n

a000041_list = map (p' 1) [0 ..]
  where
    p' = Memo.memo2 Memo.integral Memo.integral p
    p _ 0 = 1
    p k m =
      if m < k
        then 0
        else p' k (m - k) + p' (k + 1) m

-- wiki.haskell.org
partitions :: A.Array Int Integer
partitions =
  A.array (0, 1000000) $
  (0, 1) :
  [ (n, sum [s * partitions A.! p | (s, p) <- zip signs $ parts n])
  | n <- [1 .. 1000000]
  ]
  where
    signs = cycle [1, 1, (-1), (-1)]
    suite = map penta $ concat [[n, (-n)] | n <- [1 ..]]
    penta n = n * (3 * n - 1) `div` 2
    parts n = takeWhile (>= 0) [n - x | x <- suite]

problem_78 :: Int
problem_78 = head $ filter (\x -> (partitions A.! x) `mod` 1000000 == 0) [1 ..]

-- Von den Profis
sol78 =
  head . dropWhile (((/=) 0) . (flip mod 10000000) . snd) $
  zip [0 ..] $ map C.countPartitions [0 ..]

-- ---------------------- project euler 57 -----------------------------
{-
            1   3
sqrt2 = 1 + - = - = 1.5
            2   2

              1         1   5   2   7
sqrt2 = 1 + ----- = 1 + - = - + - = - = 1.4
                1       5   5   5   5
            2 + -       -
                2       2

                1             1           1      12   5    17
sqrt2 = 1 + --------- = 1 + ----- = 1 + ------ = -- + -- = -- = 1.41666
                  1             2       10   2   12   12   12
            2 + -----       2 + -       -- + -
                    1           5        5   5
                2 + -
                    2

                1                  1      29   12   41
sqrt2 = 1 + ------------- = 1 + ------- = -- + -- = -- = 1.41379
                  1             24   5    29   29   29
            2 + ---------       -- + --
                      1         12   12
                2 + -----
                        1
                    2 + -
                        2

                1                      1      70   29   99
sqrt2 = 1 + ----------------- = 1 + ------- = -- + -- = -- = 1.41379
                  1                 58   12   70   70   70
            2 + -------------       -- + --
                      1             29   29
                2 + ---------
                        1
                    2 + -----
                            1
                        2 + -
                            2
-}
sqrt2' :: [Rational]
sqrt2' = 3 % 2 : next sqrt2'
  where
    next (x:xs) =
      (numerator x + 2 * denominator x) % (numerator x + denominator x) :
      next xs

numeratorLongerDenominator :: Rational -> Bool
numeratorLongerDenominator r = n > d
  where
    n = length . show $ numerator r
    d = length . show $ denominator r

sol57' = length . filter numeratorLongerDenominator $ take 1000 sqrt2'

sqrt2 :: [(Integer, Integer)]
sqrt2 = (3, 2) : next sqrt2
  where
    next ((n, d):xs) = (n + 2 * d, n + d) : next xs

sqrt2'' :: [(Integer, Integer)]
sqrt2'' = iterate (\(n, d) -> (n + 2 * d, n + d)) (3, 2)

nLd :: (Integer, Integer) -> Bool
nLd (n, d) = (length . show) n > (length . show) d

sol57 = length . filter nLd $ take 1000000 sqrt2

-- ---------------------- project euler 58 -----------------------------
spiralcorners :: [(Integer, Integer, Rational)]
spiralcorners =
  [ ( 1 + (i - 2) * 4 + x
    , (i - 1) * 2 + 1
    , val $ 4 * i ^ 2 - (10 - 2 * x) * i + 7 - 2 * x)
  | i <- [2 ..]
  , x <- [0 .. 3]
  ]
  where
    val v
      | P.isPrime v = 1
      | otherwise = 0

sol58 = dropWhile ((> 0.1) . (\(_, _, v) -> v)) $ scanl1 nAvg spiralcorners

nAvg ::
     (Integer, Integer, Rational)
  -> (Integer, Integer, Rational)
  -> (Integer, Integer, Rational)
nAvg (n1, l1, v1) (n2, l2, v2) = (n2, l2, (n1' * v1 + v2) * (recip n2'))
  where
    n1' = toRational n1
    n2' = toRational n2

-- ---------------------- project euler 60 -----------------------------
sol60 =
  [ (p1, p2, p3, p4, p5, p1 + p2 + p3 + p4 + p5)
  | (p1, m1) <- infPrimes
  , (p2, m2) <- takeWhile ((< p1) . fst) $ infPrimes
  , (p3, m3) <- takeWhile ((< p2) . fst) $ infPrimes
  , (p4, m4) <- takeWhile ((< p3) . fst) $ infPrimes
  , (p5, m5) <- takeWhile ((< p4) . fst) $ infPrimes
  , testPrimes (p5, m5) (p4, m4)
  , testPrimes (p5, m5) (p3, m3)
  , testPrimes (p5, m5) (p2, m2)
  , testPrimes (p5, m5) (p1, m1)
  , testPrimes (p4, m4) (p3, m3)
  , testPrimes (p4, m4) (p2, m2)
  , testPrimes (p4, m4) (p1, m1)
  , testPrimes (p3, m3) (p2, m2)
  , testPrimes (p3, m3) (p1, m1)
  , testPrimes (p2, m2) (p1, m1)
  ]

infPrimes :: [(Integer, Integer)]
infPrimes = map ((\p -> (p, magnitude p)) . Pr.unPrime) Pr.primes

testPrimes :: (Integer, Integer) -> (Integer, Integer) -> Bool
testPrimes (p1, m1) (p2, m2) =
  P.isPrime (p1 + p2 * 10 ^ m1) && P.isPrime (p2 + p1 * 10 ^ m2)

magnitude :: Integer -> Integer
magnitude 0 = 0
magnitude i = 1 + magnitude (i `div` 10)

-- Haskell wiki
problem_60 = print $ (sum . head) $ solve

--isPrime x = x==3 || millerRabinPrimality x 2
solve = do
  a <- primesTo10000
  let m = f a $ dropWhile (<= a) primesTo10000
  b <- m
  let n = f b $ dropWhile (<= b) m
  c <- n
  let o = f c $ dropWhile (<= c) n
  d <- o
  let p = f d $ dropWhile (<= d) o
  e <- p
  return [a, b, c, d, e]
  where
    f x =
      filter
        (\y ->
           and
             [ P.isPrime $read $shows x $show y
             , P.isPrime $read $shows y $show x
             ])

primesTo10000 = 2 : filter P.isPrime [3,5 .. 9999]

-- ---------------------- project euler 61 -----------------------------
triangles = [n * (n + 1) `div` 2 | n <- [45 .. 140], n `mod` 100 > 9]

squares = [n ^ 2 | n <- [32 .. 99], n `mod` 100 > 9]

pentagonals = [n * (3 * n - 1) `div` 2 | n <- [26 .. 81], n `mod` 100 > 9]

hexagonals = [n * (2 * n - 1) | n <- [23 .. 70], n `mod` 100 > 9]

heptagonals = [n * (5 * n - 3) `div` 2 | n <- [21 .. 63], n `mod` 100 > 9]

octagonals = [n * (3 * n - 2) | n <- [19 .. 58], n `mod` 100 > 9]

digitsToPair i = (u, i - 100 * u)
  where
    u = i `div` 100

filterSeq xs [] = xs
filterSeq xs ys = filter ((`elem` fstYs) . snd) xs
  where
    fstYs = map fst ys

head' [] = []
head' (x:xs) = x

testFourDigitSeq xs = foldr (\x acc -> filterSeq x (head' acc) : acc) [] xs

imperative = do
  print "Hallo"
  let a = 5
  print a
  let a = 4
  print a

middleCharacters :: String -> String
middleCharacters [] = []
middleCharacters [a] = [a]
middleCharacters xs
  | length xs `mod` 2 == 0 = [last x1] ++ [head x2]
  | otherwise = [head x2]
  where
    l2 = length xs `div` 2
    x1 = take l2 xs
    x2 = drop l2 xs

-- ---------------------- project euler 62 -----------------------------
--cubes :: [(String, Integer)]
--cubes = foldr (\e -> alterOrInsert (integerToSortedString e*e*e) e
--        $ [1..1000]
alterOrInsert ::
     [Integer]
  -> Map String (Integer, [Integer])
  -> Map String (Integer, [Integer])
alterOrInsert [] m = m
--alterOrInsert (i:is) m | Map.member k m = alterOrInsert is $ Map.adjust (\(x, y) -> (x+1, i:y)) k m
alterOrInsert (i:is) m
  | Map.member k m && (c == 4) = Map.adjust (\(x, y) -> (x + 1, i : y)) k m
  | Map.member k m =
    alterOrInsert is $ Map.adjust (\(x, y) -> (x + 1, i : y)) k m
  | otherwise = alterOrInsert is $ Map.insert k (1, [i]) m
  where
    k = integerToSortedString $ i * i * i
    Just (c, vs) = Map.lookup k m

integerToSortedString :: Integer -> String
integerToSortedString i = X.sort . show $ i

sol62 =
  (^ 3) . last . snd . head . filter ((> 4) . fst) . map (snd) . Map.toList $
  alterOrInsert [1 .. 100000] Map.empty

-- ---------------------- project euler 63 -----------------------------
sods :: Integer -> Integer
sods 0 = 0
sods i = i `mod` 10 + sods (i `div` 10)
