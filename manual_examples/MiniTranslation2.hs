module MiniTranslation2 where

-- Testing for point-free style in this example
filterAndFoldInts :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterAndFoldInts filt f zero = (foldr f zero) . (filter filt)

