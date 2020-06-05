module Foldl where
    
foldl' :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldl' f base [] = base
foldl' f base (x:xs) = foldl' f (f base x) xs
