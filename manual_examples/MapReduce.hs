module MapReduce where

mapreduce :: (Int -> Int) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
mapreduce f combinator base = (foldl combinator base) . (map f)
