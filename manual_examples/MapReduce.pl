:- module(_,_,[functional, hiord]).
:- use_module(library(lists)).

% Example query
% ?- mapreduce(plus(1), plus, 0, [1,2,3], X).
% X = 9 ?

%%%%% Original Haskell function
% mapreduce :: (Int -> Int) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
% mapreduce f combinator base = (foldl combinator base) . (map f)

% Ciao's functional approach
%   |
%   v

plus(X,Y) := ~(X+Y).

:- meta_predicate mapreduce(pred(2),pred(3),?,?,?).
mapreduce(F, Combinator, Base, List) := ~compose(
	([Combinator,Base] -> ''(X,Y) :- foldl(Combinator, Base, X,Y)),
	([F] -> ''(X,Y) :- map(F,X,Y)),
	 List).

:- meta_predicate compose(pred(2),pred(2),?,?).
compose(F, G, X) := ~F(~G(X)).

:- meta_predicate map(pred(2),?,?).
map(_, []) := [].
map(F, [X|Xs]) := [~F(X) | ~map(F, Xs)].

:- meta_predicate foldl(pred(3),?,?,?).
foldl(_, Base, []) := Base.
foldl(F, Base, [X|Xs]) := ~foldl(F, ~F(Base, X), Xs).

%%%%%%%%%%

% "Pure" approach
%   |
%   v

% mapreduce(F, Combinator, Base, List, Out) :-
%     compose(myfoldr(Combinator, Base), map(F), List, Out).

% plus(X,Y,Z) :- Z is X+Y.
% plus1(Y,Z) :- Z is 1+Y.

% map(_, [], []).
% map(F, [X|Xs], [Y|Ys]) :-
%     Goal =.. [F, X, Y],
%     call(Goal),
%     map(F, Xs, Ys).
    
% compose(Fpart, Gpart, X, FGX) :-
% 	Gpart =.. ListGpart,
% 	append(ListGpart, [X, GX], ListGoal1),
% 	Goal1 =.. ListGoal1,
% 	call(Goal1),
% 	Fpart =.. ListFpart,
%         append(ListFpart, [GX, FGX], ListGoal2),
% 	Goal2 =.. ListGoal2,
% 	call(Goal2).

% myfoldr(_, B, [], B).
% myfoldr(F, B, [A|As], R) :-
% 	myfoldr(F, B, As, R1),
% 	Goal =.. [F, A, R1, R],
% 	call(Goal).
