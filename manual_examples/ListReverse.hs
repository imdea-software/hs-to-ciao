module ListReverse where

import Prelude hiding (reverse)
import Data.List (intercalate)
    
reverse :: [Int] -> [Int]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
