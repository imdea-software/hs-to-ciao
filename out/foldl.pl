:- module(_,_,[assertions, regtypes, functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

:- meta_predicate foldl_(pred(3),?,?,?).
foldl_(F, Base, []) := Base.
foldl_(F, Base, [X | Xs]) := ~foldl_(F, ~F(Base, X), Xs).


