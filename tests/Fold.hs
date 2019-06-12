module Fold where 

fold :: (a -> b -> b) -> b -> [a] -> b     
fold f base [] = base
fold f base (x:xs) = f x (fold f base xs) 
{-


sum x y = x + y 

sum x = \y -> x + y 
sum = \x y -> x + y 



foo :: Bool -> a -> Bool
foo = \x y -> if foo x y  then x else x 

bar = foo False 2 

-- foo = \ (@ a_aWm) (@ b_aWm) (x_aVO :: a_aWm) (y_aVO :: b_aWm) -> x_aVO





fold :: (a -> b -> b) -> b -> [a] -> b     
fold = \f base ds -> 
  case ds of 
    [] -> base
    (x:xs) = f x (fold f base xs) 



fold :: (a -> b -> b) -> b -> [a] -> b     
fold f base ds = 
  case ds of 
    [] -> base
    (x:xs) = f x (fold f base xs) 




-}
{-
fold(FAWY, BASEAWZ, [], BASEAWZ).
fold(F, ACK, [X|Xs], Z) :- 
  fold(F, ACK, Xs, Z1),
  G =.. [F, X, Z1, Z],
  call(G).


-}


{- 
fold (+) 0 [1, 2, 3]
= (+) 1 (fold (+) 0 [2, 3])
= (+) 1 ((+) 2 (fold (+) 0 [3]))
= (+) 1 ((+) 2 ((+) 3 (fold (+) 0 [])))
= (+) 1 ((+) 2 ((+) 3 0)))
-}
{-

fold(F, ACK, [], ACK).
fold(F, ACK, [X|Xs], Z) :- 
  fold(F, ACK, Xs, Z1),
  call(F,X, Z1, Z)
-}