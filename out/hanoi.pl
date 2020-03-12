:- module(_,_,[assertions, regtypes, functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').



:- meta_predicate hanoi(?,?,?,?,?).
hanoi(0, Ds_d2xq, Ds_d2xr, Ds_d2xs) := [].
hanoi(Ds_d2xp, Ds_d2xq, Ds_d2xr, Ds_d2xs) := ~append(~hanoi(~substract(Ds_d2xp, 1), Ds_d2xq, Ds_d2xs, Ds_d2xr), ~append([[Ds_d2xq | [Ds_d2xr | []]]], ~hanoi(~substract(Ds_d2xp, 1), Ds_d2xs, Ds_d2xr, Ds_d2xq))).
