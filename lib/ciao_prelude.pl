:- module(_,_,[functional, hiord]).

:- meta_predicate bool(goal,?).
bool(X,T) :- (X -> T=true ; T=false).

:- meta_predicate flip(pred(3),?,?,?).
flip(F,X,Y) := ~F(Y,X).

:- meta_predicate compose(pred(2),pred(2),?,?).
compose(F, G, X) := ~F(~G(X)).