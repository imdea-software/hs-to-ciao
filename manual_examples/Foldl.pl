:- module(_,_,[functional, hiord]).

:- meta_predicate foldl_(pred(3),?,?,?).
foldl_(F, Base, []) := Base.
foldl_(F, Base, .(X,Xs)) := ~foldl_(F, ~F(Base, X), Xs).
