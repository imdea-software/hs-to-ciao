:- module(_,_,[functional, hiord]).
:- use_module(library(lists)).

plus(X,Y) := ~(X+Y).

:- meta_predicate mapInts(pred(2),?,?).
mapInts(_, []) := [].
mapInts(F, [X|Xs]) := [~F(X) | ~mapInts(F, Xs)].

% Example query
% ?- mapInts(plus(2), [1,2,3], X).
% X = [3,4,5] ?