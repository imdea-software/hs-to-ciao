module MapReduce where

import Prelude hiding (foldl, (.), map)

foldl :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldl _ b []     = b
foldl f b (x:xs) = x `f` foldl f b xs

(.) :: ([Int] -> Int) -> ([Int] -> [Int]) -> [Int] -> Int
(.) f g = \x -> f (g x)

map :: (Int -> Int) -> [Int] -> [Int]
map _ [] = []
map f (x:xs) = (f x) : xs

mapreduce :: (Int -> Int) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
mapreduce f combinator base x = ((foldl combinator base) . (map f)) x
