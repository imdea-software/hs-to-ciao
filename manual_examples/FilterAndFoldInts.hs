module FilterAndFoldInts where

import Prelude hiding (foldl, (.), filter)

foldl :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldl _ b []     = b
foldl f b (x:xs) = x `f` foldl f b xs

(.) :: ([Int] -> Int) -> ([Int] -> [Int]) -> [Int] -> Int
(.) f g = \x -> f (g x)

filter :: (Int -> Bool) -> [Int] -> [Int]
filter _ [] = []
filter pred (x:xs) = if pred x then x : (filter pred xs) else filter pred xs
  -- | pred x = x : filter pred xs
  -- | otherwise = filter pred xs
          
-- Testing for point-free style in this example
filterAndFoldInts :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterAndFoldInts filt f base x = ((foldl f base) . (filter filt)) x


                                
