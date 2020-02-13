module FoldlADT where

data List = EmptyList | Cons Int List
    
foldlADT :: (Int -> Int -> Int) -> Int -> List -> Int
foldlADT f base EmptyList = base
foldlADT f base (Cons x xs) = foldlADT f (f base x) xs
