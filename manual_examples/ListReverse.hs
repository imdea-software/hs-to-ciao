module ListReverse where
    
-- foldl' :: ([Int] -> Int -> [Int]) -> [Int] -> [Int] -> [Int]
-- foldl' f base [] = base
-- foldl' f base (x:xs) = foldl' f (f base x) xs
    
-- reverse' :: [Int] -> [Int]
-- reverse' list = foldl' (flip (:)) ([] :: [Int]) list

reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
