:- module(_,_,[assertions, regtypes, functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').



:- meta_predicate lessint(?,?,?).
lessint(X, Y) := ~less(X, Y).

:- meta_predicate insert(?,?,?).
insert(X, []) := [X].
insert(X, [Y | Ys]) := (~lessint(X, Y) = true ? [X | [Y | Ys]]
| ~lessint(X, Y) = false ? [Y | ~insert(X, Ys)]).

:- meta_predicate isort(?,?).
isort([]) := [].
isort([X | Xs]) := ~insert(X, ~isort(Xs)).
