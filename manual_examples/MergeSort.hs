module MergeSort where

import Prelude hiding (take, drop)
    
take :: Int -> [Int] -> [Int]
take _ [] = []
take 0 list = []
take n (x:xs) = x:(take (n-1) xs)

drop' :: Int -> [Int] -> [Int]
drop' i [] = []
drop' i (y:ys) = if i == 0 then y:ys else drop' (i-1) ys 
                
-- drop :: Int -> [Int] -> [Int]
-- drop _ [] = []
-- drop 0 list = list
-- drop n (x:xs) = drop (n-1) xs

mergeSort :: ([Int] -> [Int] -> [Int]) -> [Int] -> [Int]
mergeSort merge xs
        | length xs < 2 = xs
        | otherwise = merge (mergeSort merge first) (mergeSort merge second)
        where first = take half xs 
              second = drop' half xs 
              half = length xs `div` 2

merge :: [Int] -> [Int] -> [Int]
merge = undefined
-- merge [] ys         = ys
-- merge xs []         = xs
-- merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
--                     | otherwise = y:merge (x:xs) ys
