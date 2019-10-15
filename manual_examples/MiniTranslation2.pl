:- module(_,_,[functional, hiord]).
:- use_module(library(lists)).

greater(X,Y) :- X > Y.
greater5(X) :- X > 5.
sum(X,Y,Z) :- Z is X+Y.

compose(Fpart, Gpart, X, FGX) :-
	Gpart =.. ListGpart,
	append(ListGpart, [X, GX], ListGoal1),
	Goal1 =.. ListGoal1,
	call(Goal1),
	Fpart =.. ListFpart,
        append(ListFpart, [GX, FGX], ListGoal2),
	Goal2 =.. ListGoal2,
	call(Goal2).

myfilter(_, [], []).
myfilter(P, [A0|As0], As) :-
	% Goal =.. [P, A0],
	(P(A0) -> As = [A0|As1] ; As = As1),
	 myfilter(P, As0, As1).

myfoldr(_, B, [], B).
myfoldr(F, B, [A|As], R) :-
	myfoldr(F, B, As, R1),
	Goal =.. [F, A, R1, R],
	call(Goal).

filterAndFoldInts(Filt, F, Zero, List, Out) :-
	compose(myfoldr(F, Zero), myfilter(Filt), List, Out).