module Append where
   
len :: [a] -> Int 
len [] = 0 
len (x:xs) = 1 + len xs 

{- 
myappend :: [a] -> [a] -> [a]
myappend [] ys = ys 
myappend (x:xs) ys = x:myappend xs ys 

nrepeat :: a -> [a]
nrepeat x = x:nrepeat x

mhead (x:xs) = x 

msort :: Ord a => [a] -> [a]
msort xs = merge (msort xs1) (msort xs2)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs 
merge (x:xs) (y:ys) 
  | x <= y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys  
-}
