module SumThreeList where

sumthreelist :: [Integer] -> [Integer]
sumthreelist [] = []
sumthreelist (x:xs) = (x + 3):(sumthreelist xs)
