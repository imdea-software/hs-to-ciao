module Append where

append :: [Int] -> [Int] -> [Int]
append [] ys = ys
append (x:xs) ys = x:(append xs ys)
