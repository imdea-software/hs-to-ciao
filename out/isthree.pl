:- module(_,_,[functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

:- meta_predicate isthree(?,?).
isthree(3) := true.
isthree(Ds_d2no) := false.


