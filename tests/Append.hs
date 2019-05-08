module Append where
 
myappend :: [a] -> [a] -> [a]
myappend [] ys = ys 
myappend (x:xs) ys = x:myappend xs ys 

myhead (x:_) = x 
mytail (_:xs) = xs 
