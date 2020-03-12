:- module(_,_,[assertions, regtypes, functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

:- meta_predicate mapints(pred(2),?,?).
mapints(Ds_d2vg, []) := [].
mapints(Ds_d2vg, [X | Xs]) := [~Ds_d2vg(X) | ~mapints(Ds_d2vg, Xs)].


