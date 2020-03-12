:- module(_,_,[assertions, regtypes, functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').



:- meta_predicate factorial(?,?).
factorial(0) := 1.
factorial(Ds_d2so) := ~mult(Ds_d2so, ~factorial(~substract(Ds_d2so, 1))).

:- meta_predicate listfactorials(?,?,?).
listfactorials(Start, End) := ~map(factorial, ~enumfromto(Start, End)).
