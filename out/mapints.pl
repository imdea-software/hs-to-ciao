:- module(_,_,[functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

:- meta_predicate mapInts(pred(2),?,?).
mapInts(Ds_d2oz, []) := [].
mapInts(Ds_d2oz, [X | Xs]) := [~Ds_d2oz(X) | ~mapInts(Ds_d2oz, Xs)].


