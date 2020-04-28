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
