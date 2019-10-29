:- module(_,_,[functional, hiord]).
:- use_module(library(lists)).

%%%%% Original Haskell function
% mapreduce :: (Int -> Int) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
% mapreduce f combinator base = (foldl combinator base) . (map f)

% Ciao's functional approach
%   |
%   v

plus(X,Y) := ~(X+Y).
plus1(X) := ~(X+1).

mapreduce(F, Combinator, Base, List) := ~compose(foldl(Combinator, Base), ~map(F, List), List).

compose(F, G, X) := ~F(~G(X)).

map(_, []) := [].
map(F, [X|Xs]) := [~F(X) | ~map(F, Xs)].

foldl(_, Base, []) := Base.
foldl(F, Base, [X|Xs]) := ~foldl(F, ~F(Base, X), Xs).

% Test query:
% mapreduce(plus1, plus, 0, [1,2,3], X).

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
