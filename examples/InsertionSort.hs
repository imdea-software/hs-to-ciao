module InsertionSort where

-- (<) :: Int -> Int -> Bool

lessInt :: Int -> Int -> Bool
lessInt x y = x < y 

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
    | x `lessInt` y = x:y:ys
    | otherwise = y:(insert x ys)

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)
