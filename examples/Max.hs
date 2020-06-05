module Max where

data MyType = White | Red | Black

instance Eq MyType where
    White == White = True
    Red == Red = True
    Black == Black = True
    _ == _ = False
            
instance Ord MyType where
    White <= Red = True
    White <= Black = True
    Red <= Black = True
    x <= y = x == y
    
max' :: Ord a => a -> a -> a
max' x y
    | y <= x = x
    | otherwise = y

-- max2 :: MyType -> MyType -> MyType
-- max2 x y = max' x y

-- max3 :: MyType -> MyType -> MyType
-- max3 x y = max'' (DOrd { lessOrEqual = (\x y -> ...) }) x y

-- data DOrd a = DOrd { lessOrEqual :: a -> a -> Bool }
                  
-- max'' :: DOrd a -> a -> a -> a
-- max'' lt x y
--     | (lessOrEqual lt) y x = x
--     | otherwise = y

