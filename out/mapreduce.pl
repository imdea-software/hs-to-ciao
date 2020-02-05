:- module(_,_,[functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

:- meta_predicate foldl(pred(3),?,?,?).
foldl(Ds_d2rt, B, []) := B.
foldl(Ds_d2rt, B, [X | Xs]) := ~Ds_d2rt(X, ~foldl(Ds_d2rt, B, Xs)).

:- meta_predicate compose(pred(2),pred(2),?,?).
compose(F, G, X) := ~F(~G(X)).

:- meta_predicate map(pred(2),?,?).
map(Ds_d2rj, []) := [].
map(Ds_d2rj, [X | Xs]) := [~Ds_d2rj(X) | ~map(Ds_d2rj, Xs)].

:- meta_predicate mapreduce(pred(2),pred(3),?,?,?).
mapreduce(F, Combinator, Base, X) := ~compose(foldl(Combinator, Base), map(F), X).


