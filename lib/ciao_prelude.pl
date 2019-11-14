:- module(_,_,[functional, hiord]).

:- meta_predicate bool(goal,?).
bool(X,T) :- (X -> T=true ; T=false).