:- module(_,_,[functional, hiord]).
:- use_module(library(lists)).

plus(X,Y) := ~(X+Y).
greater(X,Y) :- X>Y.

% NOTE: pred(1) instead of pred(2) because there are no booleans,
% have to take a closer look at this particularity
:- meta_predicate filterAndFoldInts(pred(1),pred(3),?,?,?).
filterAndFoldInts(Filt, F, Base, List) := ~compose(
	([F, Base] -> ''(X,Y) :- foldl(F, Base, X, Y)),
	([Filt] -> ''(X,Y) :- filter(Filt, X, Y)),
	 List).

% Test query
% ?- filterAndFoldInts(greater(5), plus, 0, [1,2,3,7,7,2,6], X).
% X = 20 ?

% NOTE: pred(1) instead of pred(2) because there are no booleans,
% have to take a closer look at this particularity
:- meta_predicate filter(pred(1),?,?).
filter(_, []) := [].
filter(P, [X|Xs]) := ~P(X) ? [X | ~filter(P, Xs)]
	           | ~filter(P, Xs). % otherwise

% myfilter(_, [], []).
% myfilter(P, [A0|As0], As) :-
% 	% Goal =.. [P, A0],
% 	(P(A0) -> As = [A0|As1] ; As = As1),
% 	 myfilter(P, As0, As1).
 
:- meta_predicate foldl(pred(3),?,?,?).
foldl(_, Base, []) := Base.
foldl(F, Base, [X|Xs]) := ~foldl(F, ~F(Base, X), Xs).

:- meta_predicate compose(pred(2),pred(2),?,?).
compose(F, G, X) := ~F(~G(X)).