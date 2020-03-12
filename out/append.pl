:- module(_,_,[assertions, regtypes, functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

:- meta_predicate append(?,?,?).
append([], Ys) := Ys.
append([X | Xs], Ys) := [X | ~append(Xs, Ys)].


