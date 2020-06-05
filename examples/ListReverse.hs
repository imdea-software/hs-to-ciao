module ListReverse where

import Prelude hiding (reverse)
    
reverse :: [Int] -> [Int]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
