module FoldlADT where

data MyList = EmptyList | Cons Int MyList
    
foldlADT :: (Int -> Int -> Int) -> Int -> MyList -> Int
foldlADT f base EmptyList = base
foldlADT f base (Cons x xs) = foldlADT f (f base x) xs
                              
