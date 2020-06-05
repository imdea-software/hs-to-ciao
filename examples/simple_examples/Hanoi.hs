module Hanoi where

hanoi :: Int -> Int -> Int -> Int -> [[Int]]
hanoi 0 _ _ _ = []
-- a, b and c are just numbers representing the sticks
hanoi n a b c = hanoi (n-1) a c b ++ [[a, b]] ++ hanoi (n-1) c b a
