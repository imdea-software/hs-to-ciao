module ListsMap where

listmap :: (a -> b) -> [a] -> [b]
listmap _ [] = []
listmap f (x:xs) = (f x):(listmap f xs)
