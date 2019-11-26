module FilterAndFoldInts where

-- Testing for point-free style in this example
filterAndFoldInts :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterAndFoldInts filt f base = (foldl f base) . (filter filt)

