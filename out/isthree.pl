:- module(_,_,[assertions, regtypes, functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

:- meta_predicate isthree(?,?).
isthree(3) := true.
isthree(Ds_d2uv) := false.


