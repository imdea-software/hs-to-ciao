:- module(_,_,[functional, hiord]).

:- meta_predicate foldl(pred(3),?,?,?).
foldl(F, Base, []) := Base.
foldl(F, Base, .(X,Xs)) := ~foldl(F, ~F(Base, X), Xs).
