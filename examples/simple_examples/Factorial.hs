module Factorial where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial $ n - 1)

-- factorial_tailrec :: Int -> Int
-- factorial_tailrec 0 = 1
-- factorial_tailrec n = go 1 n
--     where
--       go acc 0 = acc
--       go acc n = go (acc * n) (n - 1)

listFactorials :: Int -> Int -> [Int]
listFactorials start end = map factorial [start..end]
