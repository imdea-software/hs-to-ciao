module Tails where

tails :: [Int] -> [[Int]]
tails [] = [[]]
tails (x : xs) = (x : xs) : (tails xs)
