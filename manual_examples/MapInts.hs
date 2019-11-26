module MapInts where

-- Testing for recursion in this example
mapInts :: (Int -> Int) -> [Int] -> [Int]
mapInts _ [] = []
mapInts f (x:xs) = (f x):(mapInts f xs)
