:- module(_,_,[functional, hiord]).

%%%%% TESTING FUNCTIONS

:- meta_predicate isthree(?,?).
isthree(3) := true.
isthree(Ds_d2tf) := false.

%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate bool(goal,?).
bool(X,T) :- (X -> T=true ; T=false).

:- meta_predicate fun_apply(pred(2),?,?).
fun_apply(F, X) := ~F(X).

:- meta_predicate flip(pred(3),?,?,?).
flip(F,X,Y) := ~F(Y,X).

:- meta_predicate compose(pred(2),pred(2),?,?).
compose(F, G, X) := ~F(~G(X)).

%%%%%% Comparison operations

:- meta_predicate less(?,?,?).
less(X,Y) := ~bool(X < Y).

:- meta_predicate great(?,?,?).
great(X,Y) := ~bool(X > Y).

:- meta_predicate less_or_eq(?,?,?).
less_or_eq(X,Y) := ~bool(X =< Y).

:- meta_predicate great_or_eq(?,?,?).
great_or_eq(X,Y) := ~bool(X >= Y).

%%%%%% Arithmetic operations
:- meta_predicate sum(?,?,?).
sum(X, Y) := X + Y.

:- meta_predicate substract(?,?,?).
substract(X, Y) := X - Y.

:- meta_predicate div(?,?,?).
div(X, Y) := X // Y.

:- meta_predicate mult(?,?,?).
mult(X, Y) := X * Y.

%%%%%%%

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

:- meta_predicate append(?,?,?).
append([], X) := X.
append([H | X], Y) := [H | ~append(X,Y)].

:- meta_predicate enumfromto(?,?,?).
enumfromto(End, End) := [End].
enumfromto(Start, End) := [Start | ~enumfromto(Start + 1, End)].
    