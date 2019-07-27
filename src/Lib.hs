module Lib
    ( someFunc
    ) where

import qualified Data.Sort
import qualified Math.NumberTheory.Primes
import qualified Math.NumberTheory.Primes.Testing as P
--import qualified Math.NumberTheory.Primes.Factorisation as F
--import qualified Math.NumberTheory.Primes.Factorisation
import qualified System.IO
import qualified Data.List as L
import qualified Data.List.Extra as X
import qualified Data.Ord
import qualified Data.Function (on)
--import qualified Data.Set as Set
import qualified Numeric
import qualified Data.Char
import qualified Data.Text as T
import Data.MemoCombinators (memo2, integral) 


someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

sol51 x | x `mod` 2 == 0 = sol51 $ x + 1
        | bix /= []   = bix
        | otherwise = sol51 $ x + 2  
  where
        bix = filter (\l -> 8 <= (sum $ map (oneForTrue . P.isPrime) l)) $
              (map . map) (fromIntegral .
                           read .
                           (stringLast xs)) $
              (\biii -> [[X.replace "1" c b | c <- map show [0..9]] | b <- biii]) $
              map ((justRight (lx - 1)) .
                    (X.replace "0" "_") .
                    decToBin) $ [1..2^(lx - 1) - 1] 
        lx = length xs
        xs = show x
sol51' x | bix /= []   = (x, bix)
         | not $ P.isPrime x = sol51' $ x + 2 
         | otherwise = sol51' $ x + 2  
  where
        bix = filter (\l -> 8 <= (sum $ map (oneForTrue . P.isPrime) l)) $
              filter (x `elem`) $
              (map . map) (fromIntegral .
                           read .
                           (stringLast xs)) $
              (\biii -> [[X.replace "1" c b | c <- map show [0..9]] | b <- biii]) $
              map ( (justRight (lx - 1)) .
                    (X.replace "0" "_") .
                    decToBin) $ [1..2^(lx - 1) - 1] 
        lx = length xs
        xs = show x

--[[121313,222323,323333,424343,525353,626363,727373,828383,929393]]
--  _2_3_3 ___3_3 _2____ _2_3_3 _2_3_3
--  120303
-- check8Primes times mask numb 

decToBin' :: Integral i => i -> [Char]
decToBin' 0 = "0"
decToBin' d | d `mod` 2 == 1 = (decToBin' $ d `div` 2) ++ "1"
            | d `mod` 2 == 0 = (decToBin' $ d `div` 2) ++ "0"
decToBin d = tail $ decToBin' d              

justRight n ss | n > lss   = (take (n - lss) $ repeat '_') ++ ss
               | otherwise = ss
  where lss = length ss
oneForTrue :: Bool -> Integer
oneForTrue x | x = 1
             | otherwise = 0
charLast c d | d == '_'   = c
             | otherwise = d
stringLast = zipWith' charLast
zipWith' _ [] [] = []
zipWith' f []     (y:ys) = y     : zipWith' f [] ys
zipWith' f (x:xs) []     = x     : zipWith' f xs [] 
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


-- ---------------------- project euler 52 -----------------------------
sol52 = filter (\(a,b) -> b) $
        map (\(a, b) -> (a, allTheSame b)) $
        map (\(a,b) -> (a, map (X.sort . X.nubOrd . show) b)) $
        [(x, [2 * x, 3 * x, 4 * x, 5 * x, 6 * x]) | x <- [1..]]

allTheSame [x, y] = (==) x y
allTheSame (x1:x2:xs) = (==) x1 x2 && allTheSame (x2:xs)
-- ---------------------- project euler 53 -----------------------------
fact 0 = 1
fact n = foldr1 (*) [1..n]
combinations' 1 1 = 1
combinations' n r = fact n `div` fact r `div` fact (n - r)
sol53 = length [combinations' n r | n <- [1..100], r <- [1..n], combinations' n r > 1000000]
-- ---------------------- project euler 55 -----------------------------
isPalindrome a = (==) a $ reverse a 
intIsPalindrome i = isPalindrome $ show i
isLychrel n i | n == 0              = True
              | intIsPalindrome ly = False
              | otherwise          = isLychrel (n - 1) ly
  where ly = i + (read $ reverse $ show i)
sol55 = length $ filter (\l -> l) $ map (isLychrel 50) [1..9999]                         -- ---------------------- project euler 56 -----------------------------
sumOfDigits :: Integer -> Integer
sumOfDigits i = foldr (+) 0 $ map ((read :: String -> Integer) . (:[])) $ show i
sol56 = last $ X.sortOn (\(_,_,c) -> c) [(a, b, sumOfDigits $ a^b) | a <- [1..100], b <- [1..100]]
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

piles :: Integer -> Integer
piles n = sum [pilesHelper n i | i <- [1..n]]
pilesHelper :: Integer -> Integer -> Integer
pilesHelper n k | k > n = 0
                | n == k = 1
                | k == 0 = 0
                | otherwise = ((pilesHelper (n-k) k) +
                               (pilesHelper (n-1) (k-1))) 

temp78 = [3,5,7,9,8,7,6]
sol78 = head . dropWhile (((/=) 0) . (flip mod 1000000) . snd) $
        zip [0..] a000041_list
        
a000041 n = a000041_list !! n
a000041_list = map (p' 1) [0..]
  where
    p' = memo2 integral integral p
    p _ 0 = 1
    p k m = if m < k then 0 else p' k (m - k) + p' (k + 1) m
