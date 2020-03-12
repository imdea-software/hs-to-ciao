:- module(_,_,[assertions, regtypes, functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').



:- meta_predicate fibonacci(?,?).
fibonacci(1) := 1.
fibonacci(0) := 0.
fibonacci(Ds_d2up) := ~sum(~fibonacci(~substract(Ds_d2up, 1)), ~fibonacci(~substract(Ds_d2up, 2))).
