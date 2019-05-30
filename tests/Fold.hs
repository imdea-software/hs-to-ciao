module Fold where 

fold :: (a -> b -> b) -> b -> [a] -> b     
fold f ack [] = ack
fold f ack (x:xs) = f x (fold f ack xs) 