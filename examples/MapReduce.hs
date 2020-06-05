module MapReduce where

mapreduce :: (Int -> Int) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
mapreduce f combinator base x = ((foldl combinator base) . (map f)) x
