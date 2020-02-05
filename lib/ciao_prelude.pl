:- module(_,_,[functional, hiord]).

%%%%% TESTING FUNCTIONS

:- meta_predicate isthree(?,?).
isthree(3) := true.
isthree(Ds_d2tf) := false.

sum(X,Y) := X+Y.

%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate bool(goal,?).
bool(X,T) :- (X -> T=true ; T=false).

:- meta_predicate flip(pred(3),?,?,?).
flip(F,X,Y) := ~F(Y,X).

:- meta_predicate compose(pred(2),pred(2),?,?).
compose(F, G, X) := ~F(~G(X)).

:- meta_predicate filter(pred(2),?,?).
filter(PRED, []) := [].
filter(PRED, .(X,XS)) :=
    ( ~PRED(X) = true ? .(X, ~filter(PRED, XS))
    | ~filter(PRED, XS)
    ).

:- meta_predicate foldl(pred(3),?,?,?).
foldl(F, Base, []) := Base.
foldl(F, Base, .(X,XS)) := ~foldl(F, ~F(Base, X), XS).

:- meta_predicate map(pred(2),?,?).
map(_, []) := [].
map(F, [X|Xs]) := [~F(X) | ~map(F, Xs)].