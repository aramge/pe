module Testmap
  ( main
  ) where

import qualified Data.Map.Strict as M

polys :: [(Int, Int, Int)]
polys   = map (\(f,s) -> (f, s `div` 100, s  - 100 * (s `div` 100)))
          . filter (\(_,s) -> (s < 10000) && (s>999) && (s `div` 10 `mod` 10 /= 0))
          . concat
          $ [[(3, n * (n+1) `div` 2),
              (4, n^2),
              (5, n * (3 * n - 1) `div` 2),
              (6, n * (2 * n - 1)),
              (7, n * (5 * n - 3) `div` 2),
              (8, n * (3 * n - 2))] | n <- [19..140]]
polymap xs = [(k, filK k)| k <- xs]
  where filK (k, _, dd) = filter (\(xk, mm, _) -> (dd == mm) && (k /= xk)) xs  
                  
--dictMM :: [(Int, Int, Int)] -> M.Map Int (Int, Int, Int)
dict = polymap polys


and' :: [Bool] -> Bool
and' [x] = x
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
--concat' = Prelude.foldr (++) []
concat' [] = [] 
concat' (x:xs) = x ++ concat' xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | y < x     = y : merge (x:xs) ys
                    | otherwise = x : y : merge xs ys

halve :: [a] -> ([a], [a])
halve xs = (take halfLength xs, drop halfLength xs)
            where halfLength = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort (x:xs) | length (x:xs) > 1 = merge (msort a) (msort b)  
             | otherwise         = [x]
  where (a,b) = halve (x:xs)

main :: IO ()
main = do
  print "Hello"
