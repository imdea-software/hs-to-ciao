module FilterAndFoldInts where

import Prelude hiding (foldl)

foldl :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldl _ b []     = b
foldl f b (x:xs) = x `f` foldl f b xs
    
-- Testing for point-free style in this example
filterAndFoldInts :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterAndFoldInts filt f base x = (foldl f base) . (filter filt) $ x


                                
